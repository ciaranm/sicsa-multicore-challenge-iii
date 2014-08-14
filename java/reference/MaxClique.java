/*
 * Sequential implementation of the max clique algorithm described in:
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
 * Large chunks of this code are stolen from Patrick Prosser's implementation,
 * which is distributed under the CRAPL licence:
 *
 *     http://www.dcs.gla.ac.uk/~pat/maxClique/distribution/
 *
 * Compile like this:
 *
 *     javac MaxClique.java
 *
 * Run like this:
 *
 *     java MaxClique brock200_1.clq
 *
 * Get the .clq files (not the .clq.b files, which are a compressed binary
 * format) from here:
 *
 *     http://www.dcs.gla.ac.uk/~pat/jchoco/clique/dimacs/DIMACS_cliques/
 */

import java.util.*;
import java.io.*;

class MaxClique {
    boolean[][] A;                        // adjacency matrix
    int n;                                // n vertices
    long numColourings;                   // count recursive calls / colourings
    int maxSize;                          // size of max clique
    ArrayList<Integer> solution_permuted; // permuted
    ArrayList<Integer> solution;          // unpermuted
    Integer[] order;                      // how has A been permuted?

    void search() {
        numColourings                        = 0;
        maxSize                              = 0;
        solution_permuted                    = new ArrayList<Integer>();
        solution                             = new ArrayList<Integer>();
        ArrayList<Integer> C                 = new ArrayList<Integer>(n);
        ArrayList<Integer> P                 = new ArrayList<Integer>(n);
        for (int i = 0 ; i < n ; i++) P.add(i);

        orderVerticesAndPermuteA();

        expand(C, P);
    }

    void orderVerticesAndPermuteA() {
        // pre-calculate degrees
        final int degree[] = new int[n];
        for (int i = 0 ; i < n ; ++i)
            for (int j = 0 ; j < n ; j++)
                if (A[i][j])
                    ++degree[i];

        // work out the vertex ordering:
        order = new Integer[n];
        for (int i = 0 ; i < n; i++) order[i] = i;

        // sort by degree, tie-breaking on vertex number
        Arrays.sort(order, new Comparator<Integer>() {
            public int compare(Integer v, Integer w) {
                if (degree[v] < degree[w] || degree[v] == degree[w] && v > w) return 1;
                return -1;
            }
        });

        // permute A
        boolean[][] newA = new boolean[n][n];
        for (int i = 0 ; i < n ; ++i)
            for (int j = 0 ; j < n ; j++)
                newA[i][j] = A[order[i]][order[j]];

        A = newA;
    }

    void expand(ArrayList<Integer> C, ArrayList<Integer> P) {
        int[] bounds = new int[P.size()];
        int[] order  = new int[P.size()];
        colourOrder(C, P, bounds, order);

        for (int i = P.size() - 1 ; i >= 0 ; --i) {
            // pick a vertex...
            int v = order[i];

            // bounds check
            if (C.size() + bounds[i] <= maxSize) return;

            // accept v
            C.add(v);

            // filter
            ArrayList<Integer> newP = new ArrayList<Integer>(n);
            for (int w : P)
                if (A[v][w])
                    newP.add(w);

            // unseat the incumbent?
            if (C.size() > maxSize) saveSolution(C);

            // recurse
            if (! newP.isEmpty()) expand(C, newP);

            // now reject v
            C.remove(C.size() - 1); /* pop */
            P.remove((Integer) v);
        }
    }

    @SuppressWarnings("unchecked")
    void colourOrder(ArrayList<Integer> C, ArrayList<Integer> P, int[] bounds, int[] order) {
        // count how many colourings we do
        numColourings++;

        ArrayList<Integer> uncoloured = (ArrayList<Integer>) P.clone();
        int currentColour             = 1;
        int position                  = 0;

        // fill colour classes...
        while (! uncoloured.isEmpty()) {
            ArrayList<Integer> colourable = (ArrayList<Integer>) uncoloured.clone();

            // fill this colour class...
            while (! colourable.isEmpty()) {
                // pick a vertex
                int v = colourable.get(0);

                // colour it
                uncoloured.remove((Integer) v);
                bounds[position] = currentColour;
                order[position] = v;
                ++position;

                // can't give adjacent things the same colour
                ArrayList<Integer> stillColourable = new ArrayList<Integer>();
                for (int w : colourable)
                    if (v != w && ! A[v][w])
                        stillColourable.add(w);
                colourable = stillColourable;
            }

            ++currentColour;
        }
    }

    void saveSolution(ArrayList<Integer> C) {
        solution_permuted.clear();
        for (int i : C) solution_permuted.add(i);

        solution.clear();
        for (int i : C) solution.add(order[i]);

        maxSize = C.size();
    }

    void readDIMACS(String fname) throws IOException {
        // Assumes the input is valid... Naughty us.
        String s = "";
        Scanner sc = new Scanner(new File(fname));
        while (sc.hasNext() && ! s.equals("p")) s = sc.next();
        sc.next(); // "edge"
        n      = sc.nextInt();
        int m  = sc.nextInt();
        A      = new boolean[n][n];
        while (sc.hasNext()) {
            s = sc.next();
            if (s.equals("e")) {
                int i = sc.nextInt() - 1;
                int j = sc.nextInt() - 1;
                A[i][j] = A[j][i] = true;
            }
            else if (s.equals("c")) {
                /* comment */
                sc.nextLine();
            }
            else
                throw new IOException("Invalid input file");
        }
        sc.close();
    }

    public static void main(String[] args)  throws IOException {
        MaxClique mc = new MaxClique();

        mc.readDIMACS(args[0]);

        System.gc();
        long cpuTime = System.currentTimeMillis();
        mc.search();
        cpuTime = System.currentTimeMillis() - cpuTime;

        System.out.println("colourings = " + mc.numColourings);

        System.out.println("cpu = " + cpuTime);

        System.out.print("order =");
        for (int v : mc.order) System.out.print(" " + (v + 1));
        System.out.println();

        System.out.println("size = " + mc.maxSize);

        Collections.sort(mc.solution_permuted);
        System.out.print("solution_permuted =");
        for (int v : mc.solution_permuted) System.out.print(" " + (v + 1));
        System.out.println();

        Collections.sort(mc.solution);
        System.out.print("solution =");
        for (int v : mc.solution) System.out.print(" " + (v + 1));
        System.out.println();
    }
}

