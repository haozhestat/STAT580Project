//The format of the data is like the given Lena dataset. 
//The first row is the dimension of the matrix, N is the row, P is the column
//And N >= P. And the data written in the file is arranged by column.
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define EPSILON 0.0001
#define MAX_TIMES 100000

void dgesvd_(char *JOBU, char *JOBVT, int *M, int *n, double *A, int *LDA, double *S, double *U, int *LDU, double *VT, int *LDVT, double *WORK, int *LWORK, int *INFO);

// The first argument is the filename, and the second argument is the value of lamda
int main(int argc, char *argv[]){
	int N,P,i,j,k,t;
	char *filename = argv[1];
	char *plamda = argv[2];
	
	double lamda;
	lamda = atof(plamda);
	FILE *fp = fopen(filename, "r");
	
	fscanf(fp, "%d ", &N);
	fscanf(fp, "%d\n", &P);
	
	double data[N*P], tempData;
	double zeroPosi[N*P];
	
	while(fscanf(fp, "%lf", &tempData) == 1)
	{
		data[i] = tempData;
		i += 1;
	}
	
	/*
	for(i=0;i<N;i++)
	{
		for(j=0;j<P;j++)
		{
			printf("%lf ", data[j*P+i]);
		}
		printf("\n");
	}
	*/
	
	for(i=0; i<N*P; i++){
		if(fabs(data[i]) < 10e-10) //Whether equals to zero
			zeroPosi[i] = 1;
		else
			zeroPosi[i] = 0;
		//printf("%lf ", zeroPosi[i]);
	}
	
	char jobu = 'A', jobvt = 'A';
	int row = N, col = P, lda = N, ldu = N, ldvt = P, lwork = 5*N*P, info, diff;
	double s[P], s_star[P], u[N*P], vt[P*P], work[lwork], SSquare_1, SSquare_2;
	
	double Z_Old[N*P],Z_New[N*P],Z_Proj[N*P],Z_Proj_Backup[N*P],UD[N*P],error=1;
	
	for(i=0;i<N*P;i++)
		Z_Old[i] = 0;
	
	i = 0;
	while(error > EPSILON && i < MAX_TIMES)
	{	
		for(j=0;j<N*P;j++){
			Z_Proj[j] = data[j] + zeroPosi[j] * Z_Old[j];
			Z_Proj_Backup[j] = data[j] + zeroPosi[j] * Z_Old[j]; //The Z_Proj will be destroyed after the SVD, Z_Proj_Backup records its values.
		}
		
		dgesvd_(&jobu, &jobvt, &row, &col, Z_Proj, &lda, s, u, &ldu, vt, &ldvt, work, &lwork, &info);
		
		for(j=0;j<P;j++){
			diff = s[j] - lamda;
			if(diff > 0)
				s_star[j] = diff;
			else
				s_star[j] = 0;
		}
		
		//Calculate Z_New
		for(j=0;j<N*P;j++)
			Z_New[j] = 0;
		
		for(j=0;j<P;j++)
		{
			for(k=0;k<N;k++)
			{
				UD[j*N+k] = s_star[j] * u[j*N+k];
			}
		}
		
		for(j=0;j<N;j++)
		{
			for(k=0;k<P;k++)
			{
				for(t=0;t<P;t++)
				{
					Z_New[k*N+j] += UD[t*N+j]*vt[k*P+t];
				}
			}
		}
		
		//Calculate Error
		if(i == 0){
			i += 1;
			continue;
		}
		
		SSquare_1 = 0;SSquare_2 = 0;
		for(j=0;j<N*P;j++)
		{
			SSquare_1 += (Z_New[j] - Z_Old[j])*(Z_New[j] - Z_Old[j]);
			SSquare_2 += Z_Old[j] * Z_Old[j];
		}
		
		error = SSquare_1/SSquare_2;
		i += 1;
		
		for(j=0;j<N*P;j++){
			Z_Old[j] = Z_New[j];
		}
	}
	
	//printf("%d\n",i); //Number of the interations.
	
	/*
	for(i=0;i<N;i++)
	{
		for(j=0;j<P;j++)
		{
			printf("%lf ",Z_Proj_Backup[j*N+i]);
		}
		printf("\n");
	}
	*/
	
	for(i=0;i<N;i++)
	{
		for(j=0;j<P;j++)
		{
			printf("%lf ",Z_New[j*N+i]);
		}
		printf("\n");
	}
	
	for(i=0;i<N;i++)
	{
		for(j=0;j<P;j++)
		{
			printf("%lf ",data[j*N+i]);
		}
		printf("\n");
	}
	
	return 0;
}