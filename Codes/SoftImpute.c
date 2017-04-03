#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#define Epsilon 0.001
#define Iteration 10000

void dgesvd_(char *jobu, char *jobvt, int *m, int *n, double *a, int *lda, double *s, double *u,
             int *ldu, double *vt, int *ldvt, double *work, int *lwork, int *info);
void dgemm_(char *transa, char *transb, int *m, int *n, int *k, double *alpha, double *a, int *lda,
            double *b, int *ldb, double *beta, double *c, int *ldc);

int main(int argc, char *argv[]) {
    //The 1st argument is the file name; the 2nd argument is lambda
    char *filename = argv[1];
    FILE *f = fopen(filename, "r");
    double lambda = atof(argv[2]);
    int N, P, i=0, j, id=0;
    
    //In the dataset, the first 2 observations are N and P
    fscanf(f, "%d", &N);
    fscanf(f, "%d\n", &P);
    double data[N*P], X[N*P], Zcomp[N*P], Ztemp[N*P], Znew[N*P], Zold[N*P];
    
    // Read in the dataset
    while (!feof(f)) {
        fscanf(f, "%lf*[^lf]", &data[i]);
        i++;
    }
    
    //Transpose(data) gives X
    for (j=0; j<P; j++) {
        for (i=0; i<N; i++) {
            X[j*N+i] = data[i*P+j];
        }
    }
    //Initialize Zold with 0
    for (i=0; i>N*P; i++) {
        Zold[i] = 0;
    }
    
    while (id < Iteration) {
        //Compute the complementary matrix of Zold
        for (i=0; i<N*P; i++) {
            if (X[i]==0) {
                Zcomp[i]=Zold[i];
            } else {
                Zcomp[i] = 0;
            }
        }
        
        //Add X and Zcomp up
        for (i=0; i<N*P; i++) {
            Ztemp[i] = X[i] + Zcomp[i];
        }
        
        char jobu='A', jobvt='A';
        double S[P], U[N*N], VT[P*P], work[5*N];
        int lwork=5*N, info;
        // Do SVD to Ztemp
        dgesvd_(&jobu, &jobvt, &N, &P, Ztemp, &N, S, U, &N, VT, &P, work, &lwork, &info);
        // Compute Dlambda
        for (i=0; i<P; i++) {
            if (S[i]-lambda>0) {
                S[i] = S[i] - lambda;
            } else {
                S[i] = 0;
            }
            
        }
        //Compute Znew
        double UD[N*P], alpha=1.0, beta=0.0;
        char transa='N', transb='N';
        for (j=0; j<P; j++) {
            for (i=0; i<N; i++) {
                UD[N*j+i] = U[N*j+i] * S[j];
            }
        }
        dgemm_(&transa, &transb, &N, &P, &P, &alpha, UD, &N, VT, &P, &beta, Znew, &N);
        
        
        //Campare the ratio and Epsilon
        double Nnorm=0, Dnorm=0, ratio;
        for (i=0; i<N*P; i++) {
            Nnorm = Nnorm + pow(Znew[i]-Zold[i],2);
            Dnorm = Dnorm + pow(Zold[i], 2);
        }
        ratio = Nnorm/Dnorm;
        if (ratio<Epsilon) break;
        //Update Zold with Znew
        for (i=0; i<N*P; i++) {
            Zold[i] = Znew[i];
        }
        id++;
    }
   //Print out the final result
    for (i=0; i<N; i++) {
        for (j=0; j<P; j++) {
            printf("%lf", Znew[j*N+i]);
        }
        printf("\n");
    }
    
    return 0;
}
