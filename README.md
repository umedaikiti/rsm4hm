# A template based synthesizer of ranking supermartingales for higher moments
This is an implementation of the method of our TACAS paper, "[Tail Probabilities for Randomized Program Runtimes via Martingales for Higher Moments](https://doi.org/10.1007/978-3-030-17465-1_8)".

Our tool calculates an upper bound of k-th moments of runtime E[T^k] of a given randomized program where k is a natural number and T is a random variable representing the runtime of the program.

Our tool supports randomized programs with
- sampling from discrete/continuous distributions, and
- (demonic) nondeterminism.

## How does it work?
Our tool reduces the problem of obtaining an upper bound to an LP/SDP problem using _ranking supermartingale for higher moments_ (or _ranking supermartingale for the k-th moment_ for some k).
Ranking supermartingales for the k-th moment are functions on the set of states of the input randomized program, whose value gives an upper bound of the k-th moment of runtime.
If there is a ranking supermartingale that can be expressed by linear (or polynomial) functions, then our tool determines the coefficients of those functions by solving an LP (or SDP) problem.
For more details, see [our paper](https://doi.org/10.1007/978-3-030-17465-1_8).

## Setup
Our tool is written in OCaml. You need [GLPK](https://www.gnu.org/software/glpk/) for linear templates and [SOSTOOLS](http://www.cds.caltech.edu/sostools/) for polynomial templates.

We tested on the following environment but our tool would also work on other platforms.
- OS: Ubuntu 18.04.1
- OCaml version 4.09.0
- menhir version 20190924
- MATLAB R2018b
- SOSTOOLS 3.03
- SDPT3 4.0

### For Ubuntu
1. Install required packages (ocaml, menhir).
    ```
    $ sudo apt install ocaml menhir
    ```

2. Compile.
    ```
    $ cd linearTemplate
    $ make
    $ cd ../polynomialTemplate
    $ make
    $ cd ../
    ```
3. Install GLPK.
    ```
    $ sudo apt install glpk-utils
    ```

4. Install SOSTOOLS.

    1. Download MATLAB from [MathWorks website](https://www.mathworks.com/downloads).

    2. Install MATLAB and Symbolic Math Toolbox  .
        Follow [Installation and Licensing
        Documentation](https://jp.mathworks.com/help/install/index.html).
        The installer command (.../Install) requires superuser privileges. Use sudo.

    3. Install SDPT3.
        ```
        $ curl -O http://www.math.nus.edu.sg/~mattohkc/SDPT3-4.0.zip
        $ unzip SDPT3-4.0.zip
        $ cd SDPT3-4.0
        $ matlab -nojvm -nodisplay -nosplash -r "Installmex(1);exit"
        $ cd ../
        ```

    4. Download SOSTOOLS.
        ```
        $ curl -O http://sysos.eng.ox.ac.uk/sostools/SOSTOOLS.303.zip
        $ unzip SOSTOOLS.303.zip
        ```


## Usage

### Input
There are several examples of input program in `sample` directory.

Remark: for a technical reason, it is recommended to have a little margin when you write conditions in if branching. (to-do: explain)

### Linear template
By running the following commands, our linear template program generates a linear programming problem.
```
$ cd artifact/linearTemplate
$ ./compile ../sample/random_walk_1d_intvalued.pp -order 2 -o random_walk_1d_intvalued.mod
```
To solve this LP problem using GLPK:
```
$ glpsol -m ./random_walk_1d_intvalued.mod --output random_walk_1d_intvalued.sol
```
The minimum value of the objective function is an upper bound of the second moment of runtime.


### Polynomial template
By running the following commands, our tool generates a sum of square problem.
```
$ cd artifact/
$ ./poly_main ../sample/random_walk_1d_intvalued.pp -deg 2 -order 2 -sosdeg 1
```
To solve the SOS problem:
```
$ matlab -nojvm -nodisplay -nosplash -r "init;random_walk_1d_intvalued_pp_poly_deg2_order2;exit"
```
Please note that it may take much time to solve SOS problems.

## Acknowledgement
We thank the authors of "[Ranking and Repulsing Supermartingales for Reachability in Probabilistic Programs](https://doi.org/10.1007/978-3-030-01090-4_28)" for sharing their implementation, on which our implementation is largely based.