/*
 *  mrqmin.h
 */

float **matrix(long nrl, long nrh, long ncl, long nch);
void free_matrix(float **m, long nrl, long nrh, long ncl, long nch);
void mrqmin(float x[], float y[], float sig[], int ndata, float a[], int ia[],
			int ma, float **covar, float **alpha, float *chisq,
			void (*funcs)(float, float [], float *, float [], int), float *alamda);