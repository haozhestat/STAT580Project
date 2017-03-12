#include<stdio.h>
#include <stdlib.h>

#define EPSILON 0.001

void dgesvd_(char *JOBU, char *JOBVT, int *M, int *n, double *A, int *LDA, double *S, double *U, 
	int *LDU, double *VT, int *LDVT, double *WORK, int *LWORK, int *INFO);

int main(int argc, char *argv[]){
	int N, P, Q;
	double LAMBDA;
	N = atoi(argv[1]);
	P = atoi(argv[2]);
	Q = atoi(argv[3]);
	LAMBDA = atof(argv[4]);
	
	double Mat[N][P];
	int MIndex[Q][2];
	FILE *fp1, *fp2;
	int i, j, k;
	fp1 = fopen(argv[5], "r");
	fp2 = fopen(argv[6], "r");
	for(i=0;i<N;i++)
		for(j=0;j<P;j++)
			fscanf(fp1, "%lf", &Mat[i][j]);

	for(i=0;i<Q;i++)
		for(j=0;j<2;j++)
			fscanf(fp2, "%d", &MIndex[i][j]);
	
	int info,LWORK=5*N*P;
	char JOBU='A', JOBVT='A';
	double Mat_t[N*P], S[P], U[N*N], VT[N*N],WORK[5*N*P];

	for(i=0;i<N;i++)
		for(j=0;j<P;j++)
			Mat_t[j*N+i] = Mat[i][j];
	
	double Z_Old[Q], Z_New[Q], Z_Diff_Norm, Z_Old_Norm, Norm_Ratio = 1;
	for(i=0;i<Q;i++){
		Z_New[i] = Mat[MIndex[i][1]][MIndex[i][2]];
		Mat[MIndex[i][1]][MIndex[i][2]] = 0;
	}
 	
	dgesvd_(&JOBU,&JOBVT,&N,&P,Mat_t,&N,S,U,&N,VT,&P,WORK,&LWORK,&info);	
	double Max_S = S[1];

	while(Norm_Ratio>EPSILON){
		dgesvd_(&JOBU,&JOBVT,&N,&P,Mat_t,&N,S,U,&N,VT,&P,WORK,&LWORK,&info);	
		for(i=0;i<P;i++){
			if(S[i]>LAMBDA) S[i] = S[i] - LAMBDA;
			else S[i] = 0;
		}
		Z_Old_Norm = 0;
		Z_Diff_Norm = 0;
		for(i=0;i<Q;i++){
			Z_Old[i] = Z_New[i];
			Z_Old_Norm += Z_Old[i]*Z_Old[i];
		}
		for(k=0;k<Q;k++){
			Mat_t[MIndex[k][2]*N+MIndex[k][1]] = 0;
			for(i=0;i<P;i++){
				Mat_t[MIndex[k][2]*N+MIndex[k][1]] += 
					U[i*N+MIndex[k][1]]*S[i]*VT[MIndex[k][2]*P+i];
			}
			Z_New[k] = Mat_t[MIndex[k][2]*N+MIndex[k][1]];
			Z_Diff_Norm += (Z_New[k] - Z_Old[k])*(Z_New[k] - Z_Old[k]);
		}	
		Norm_Ratio = Z_Diff_Norm/Z_Old_Norm;
		printf("%f %f %f\n", Z_Diff_Norm, Z_Old_Norm, S[1]);
		if(Z_Old_Norm < S[1]* EPSILON) break;
	}
	
	dgesvd_(&JOBU,&JOBVT,&N,&P,Mat_t,&N,S,U,&N,VT,&P,WORK,&LWORK,&info);
	for(i=0;i<P;i++)
		printf("%f,", S[i]);	
	
	return(0);
}
