/* vim: set sw=4 sts=4 et foldmethod=syntax : */

/*
 * Author: Ciaran McCreesh
 *
 * Voodoo haxxed C++ sequential implementation of the max clique algorithm
 * described in:
 *
 *     "Algorithms for the Maximum Clique Problem, and How to Parallelise
 *     Them", Ciaran McCreesh, June 16, 2014
 *
 * which is essentially the "MCSa1" variant of Tomita et al.'s algorithms which
 * is described in this paper, except using San Segundo et al.'s BBMC approach
 * to colouring:
 *
 *     http://www.mdpi.com/1999-4893/5/4/545
 *
 * This code is spectacularly ugly and does all sorts of naughty things that
 * shouldn't be seen in public.
 *
 * You will need GCC 4.9: earlier versions have an incomplete <regex>, and you
 * will just see "Error: regex_error" at runtime.
 *
 * Compile like this:
 *
 *     g++-4.9 -O3 -march=native -o max_clique -std=c++11 max_clique.cc
 *
 * Run like this:
 *
 *     ./max_clique brock200_1.clq
 *
 * Get the .clq files (not the .clq.b files, which are a compressed binary
 * format) from here:
 *
 *     http://www.dcs.gla.ac.uk/~pat/jchoco/clique/dimacs/DIMACS_cliques/
 */

#include <iostream>
#include <exception>
#include <cstdlib>
#include <chrono>
#include <thread>
#include <fstream>
#include <map>
#include <set>
#include <regex>
#include <algorithm>
#include <numeric>

namespace
{
    class SomethingWentWrong :
        public std::exception
    {
        private:
            std::string _what;

        public:
            SomethingWentWrong(const std::string & message) throw () :
                _what(message)
            {
            }

            auto what() const throw () -> const char *
            {
                return _what.c_str();
            }
    };

    // just until we convert it into the bitset format. first is the size,
    // second is adjacency information.
    using GraphFromFile = std::pair<unsigned, std::map<int, std::set<int> > >;

    /**
     * Read in a graph from a DIMACS format file. Produces a pair whose first
     * value is the number of vertices, and whose second is a map from vertex
     * number to a set of adjacent vertices.
     */
    auto read_dimacs(const std::string & filename) -> GraphFromFile
    {
        GraphFromFile result;

        std::ifstream infile{ filename };
        if (! infile)
            throw SomethingWentWrong{ "unable to open file" };

        std::string line;
        while (std::getline(infile, line)) {
            if (line.empty())
                continue;

            /* Lines are comments, a problem description (contains the number of
             * vertices), or an edge. */
            static const std::regex
                comment{ R"(c(\s.*)?)" },
                problem{ R"(p\s+(edge|col)\s+(\d+)\s+(\d+)?\s*)" },
                edge{ R"(e\s+(\d+)\s+(\d+)\s*)" };

            std::smatch match;
            if (regex_match(line, match, comment)) {
                /* Comment, ignore */
            }
            else if (regex_match(line, match, problem)) {
                /* Problem. Specifies the size of the graph. Must happen exactly
                 * once. */
                if (0 != result.first)
                    throw SomethingWentWrong{ "multiple 'p' lines encountered" };
                result.first = std::stoi(match.str(2));
                for (int i = 0 ; i < result.first ; ++i)
                    result.second[i];
            }
            else if (regex_match(line, match, edge)) {
                /* An edge. DIMACS files are 1-indexed. We assume we've already had
                 * a problem line (if not our size will be 0, so we'll throw). */
                int a{ std::stoi(match.str(1)) }, b{ std::stoi(match.str(2)) };
                if (0 == a || 0 == b || a > result.first || b > result.first)
                    throw SomethingWentWrong{ "line '" + line + "' edge index out of bounds" };
                else if (a == b)
                    throw SomethingWentWrong{ "line '" + line + "' contains a loop" };
                result.second[a - 1].insert(b - 1);
                result.second[b - 1].insert(a - 1);
            }
            else
                throw SomethingWentWrong{ "cannot parse line '" + line + "'" };
        }

        if (! infile.eof())
            throw SomethingWentWrong{ "error reading file" };

        return result;
    }

    using BitWord = unsigned long long;

    static const constexpr int bits_per_word = sizeof(BitWord) * 8;

    /**
     * Bitset, containing a fixed number of words which is selected at compile
     * time.
     */
    template <unsigned words_>
    class BitSet
    {
        private:
            using Bits = std::array<BitWord, words_>;

            int _size = 0;
            Bits _bits = {{ }};

        public:
            auto resize(int size) -> void
            {
                _size = size;
            }

            auto set(int a) -> void
            {
                _bits[a / bits_per_word] |= (BitWord{ 1 } << (a % bits_per_word));
            }

            auto unset(int a) -> void
            {
                _bits[a / bits_per_word] &= ~(BitWord{ 1 } << (a % bits_per_word));
            }

            auto set_all() -> void
            {
                /* only done once, not worth making it clever */
                for (int i = 0 ; i < _size ; ++i)
                    set(i);
            }

            auto test(int a) const -> bool
            {
                return _bits[a / bits_per_word] & (BitWord{ 1 } << (a % bits_per_word));
            }

            auto popcount() const -> unsigned
            {
                unsigned result = 0;
                for (auto & p : _bits)
                    result += __builtin_popcountll(p);
                return result;
            }

            auto empty() const -> bool
            {
                for (auto & p : _bits)
                    if (0 != p)
                        return false;
                return true;
            }

            auto intersect_with(const BitSet<words_> & other) -> void
            {
                for (typename Bits::size_type i = 0 ; i < words_ ; ++i)
                    _bits[i] = _bits[i] & other._bits[i];
            }

            auto intersect_with_complement(const BitSet<words_> & other) -> void
            {
                for (typename Bits::size_type i = 0 ; i < words_ ; ++i)
                    _bits[i] = _bits[i] & ~other._bits[i];
            }

            auto first_set_bit() const -> int
            {
                for (typename Bits::size_type i = 0 ; i < _bits.size() ; ++i) {
                    int b = __builtin_ffsll(_bits[i]);
                    if (0 != b)
                        return i * bits_per_word + b - 1;
                }
                return -1;
            }
    };

    /**
     * Bitset-encoded graph, using a fixed number of words which is selected at
     * compile time.
     */
    template <unsigned n_words_>
    class BitGraph
    {
        private:
            using Rows = std::vector<BitSet<n_words_> >;

            int _size = 0;
            Rows _adjacency;

        public:
            auto size() const -> int
            {
                return _size;
            }

            auto resize(int size) -> void
            {
                _size = size;
                _adjacency.resize(size);
                for (auto & row : _adjacency)
                    row.resize(size);
            }

            auto add_edge(int a, int b) -> void
            {
                _adjacency[a].set(b);
                _adjacency[b].set(a);
            }

            auto adjacent(int a, int b) const -> bool
            {
                return _adjacency[a].test(b);
            }

            auto intersect_with_row(int row, BitSet<n_words_> & p) const -> void
            {
                p.intersect_with(_adjacency[row]);
            }

            auto intersect_with_row_complement(int row, BitSet<n_words_> & p) const -> void
            {
                p.intersect_with_complement(_adjacency[row]);
            }
    };

    struct MaxCliqueResult
    {
        unsigned size = 0;

        std::set<int> members = { };

        unsigned long long n_colourings = 0;
    };

    /**
     * Sort the vertices in 'p' by degree, tie-breaking on vertex number.
     */
    auto degree_sort(const GraphFromFile & graph, std::vector<int> & p) -> void
    {
        // pre-calculate degrees
        std::vector<int> degrees;
        std::transform(p.begin(), p.end(), std::back_inserter(degrees),
                [&] (int v) { return graph.second.find(v)->second.size(); });

        // tie-break on vertex number
        std::sort(p.begin(), p.end(),
                [&] (int a, int b) { return ! (degrees[a] < degrees[b] || (degrees[a] == degrees[b] && a > b)); });
    }

    /**
     * Max clique algorithm.
     *
     * We compile this for various different numbers of words. This has two
     * benefits: it makes it easy to avoid dynamic memory allocation, and it
     * helps the compiler do loop unrolling.
     */
    template <unsigned n_words_>
    struct MaxClique
    {
        BitGraph<n_words_> graph;
        std::vector<int> order;
        MaxCliqueResult result;

        MaxClique(const GraphFromFile & g) :
            order(g.first)
        {
            // populate our order with every vertex initially
            std::iota(order.begin(), order.end(), 0);
            degree_sort(g, order);

            // re-encode graph as a bit graph
            graph.resize(g.first);

            for (int i = 0 ; i < g.first ; ++i)
                for (int j = 0 ; j < g.first ; ++j)
                    if (g.second.find(order[i])->second.count(order[j]))
                        graph.add_edge(i, j);
        }

        auto colour_class_order(
                const BitSet<n_words_> & p,
                std::array<unsigned, n_words_ * bits_per_word> & p_order,
                std::array<unsigned, n_words_ * bits_per_word> & p_bounds) -> void
        {
            BitSet<n_words_> p_left = p; // not coloured yet
            unsigned colour = 0;         // current colour
            unsigned i = 0;              // position in p_bounds

            // while we've things left to colour
            while (! p_left.empty()) {
                // next colour
                ++colour;
                // things that can still be given this colour
                BitSet<n_words_> q = p_left;

                // while we can still give something this colour
                while (! q.empty()) {
                    // first thing we can colour
                    int v = q.first_set_bit();
                    p_left.unset(v);
                    q.unset(v);

                    // can't give anything adjacent to this the same colour
                    graph.intersect_with_row_complement(v, q);

                    // record in result
                    p_bounds[i] = colour;
                    p_order[i] = v;
                    ++i;
                }
            }
        }

        auto expand(
                std::vector<unsigned> & c,
                BitSet<n_words_> & p
                ) -> void
        {
            ++result.n_colourings;

            // initial colouring
            std::array<unsigned, n_words_ * bits_per_word> p_order;
            std::array<unsigned, n_words_ * bits_per_word> p_bounds;
            colour_class_order(p, p_order, p_bounds);

            // for each v in p... (v comes later)
            for (int n = p.popcount() - 1 ; n >= 0 ; --n) {
                // bound, timeout or early exit?
                if (c.size() + p_bounds[n] <= result.size)
                    return;

                auto v = p_order[n];

                // consider taking v
                c.push_back(v);

                // filter p to contain vertices adjacent to v
                BitSet<n_words_> new_p = p;
                graph.intersect_with_row(v, new_p);

                if (new_p.empty()) {
                    if (c.size() > result.size) {
                        result.size = c.size();

                        result.members.clear();
                        for (auto & v : c)
                            result.members.insert(order[v]);
                    }
                }
                else
                    expand(c, new_p);

                // now consider not taking v
                c.pop_back();
                p.unset(v);
            }
        }

        auto run() -> MaxCliqueResult
        {
            result.size = 0;

            std::vector<unsigned> c;
            c.reserve(graph.size());

            BitSet<n_words_> p;
            p.resize(graph.size());
            p.set_all();

            // go!
            expand(c, p);

            return result;
        }
    };

    /* Start of spectacular template voodoo hackery which you shouldn't read.
     * The point of this lot is to instantiate MaxClique<> for lots of
     * different numbers of words, and then to call the appropriate one. */

    /**
     * The template arguments are a list of sizes that we going to compile for.
     * Specialisations of this template defines two members: a constant 'n',
     * which is the first item in the list, and a type 'Rest', which is either
     * GraphSizes with n removed, or NoMoreGraphSizes.
     */
    template <unsigned...>
    struct GraphSizes;

    /**
     * To indicate that we've reached the end of the GraphSizes list.
     */
    struct NoMoreGraphSizes
    {
    };

    /**
     * The template arguments are a list of sizes that we going to compile for.
     * This specialisation is used when there are at least two sizes left. The
     * first size becomes the 'n' member, and the remainder become the 'Rest'
     * member.
     */
    template <unsigned n_, unsigned... n_rest_>
    struct GraphSizes<n_, n_rest_...>
    {
        enum { n = n_ };

        using Rest = GraphSizes<n_rest_...>;
    };

    /**
     * The template arguments are a list of sizes that we going to compile for.
     * This specialisation is used when there is just one size left. The first
     * size becomes the 'n' member, and the 'Rest' member is set to
     * NoMoreGraphSizes.
     */
    template <unsigned n_>
    struct GraphSizes<n_>
    {
        enum { n = n_ };

        using Rest = NoMoreGraphSizes;
    };

    /**
     * Pick the appropriate instantiation of MaxClique for the size of 'graph',
     * and run it.
     */
    template <template <unsigned> class Algorithm_, typename Result_, unsigned... sizes_>
    auto select_graph_size(const GraphSizes<sizes_...> &, const GraphFromFile & graph) -> Result_
    {
        // Does the first graph size in the list have enough bits for our graph?
        if (graph.first < GraphSizes<sizes_...>::n * bits_per_word) {
            // Yes, run it
            Algorithm_<GraphSizes<sizes_...>::n> algorithm{ graph };
            return algorithm.run();
        }
        else {
            // No, try the next graph size in the list instead.
            return select_graph_size<Algorithm_, Result_>(typename GraphSizes<sizes_...>::Rest(), graph);
        }
    }

    /**
     * Overload of select_graph_size, for when we run out of sizes.
     */
    template <template <unsigned> class Algorithm_, typename Result_>
    auto select_graph_size(const NoMoreGraphSizes &, const GraphFromFile &) -> Result_
    {
        throw SomethingWentWrong{ "graph too big (recompile with a larger AllGraphSizes)" };
    }

    /**
     * Specify the sizes of graphs for which we're going to compile. Here, '1'
     * means '1 word', not '1 vertex'.
     */
    using AllGraphSizes = GraphSizes<1, 2, 3, 4, 5, 6, 7, 8, 12, 16, 32, 64, 128, 256, 512, 1024>;

    /* End of spectacular template voodoo hackery which you shouldn't read */

    /**
     * Convenient way to run the max clique algorithm.
     */
    auto max_clique(const GraphFromFile & graph) -> MaxCliqueResult
    {
        return select_graph_size<MaxClique, MaxCliqueResult>(AllGraphSizes(), graph);
    }
}

auto main(int argc, char * argv[]) -> int
{
    try {
        if (2 != argc) {
            std::cout << "Usage: " << argv[0] << " file" << std::endl;
            return EXIT_FAILURE;
        }

        auto graph = read_dimacs(std::string(argv[1]));

        auto start_time = std::chrono::steady_clock::now();

        auto result = max_clique(graph);

        auto overall_time = std::chrono::duration_cast<std::chrono::milliseconds>(
                std::chrono::steady_clock::now() - start_time);

        std::cout << "size = " << result.size << std::endl;

        std::cout << "solution =";
        for (auto v : result.members)
            std::cout << " " << v + 1;
        std::cout << std::endl;

        std::cout << "colourings = " << result.n_colourings << std::endl;

        std::cout << "cpu = " << overall_time.count() << std::endl;

        return EXIT_SUCCESS;
    }
    catch (const std::exception & e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return EXIT_FAILURE;
    }
}

