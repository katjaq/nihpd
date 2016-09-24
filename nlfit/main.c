#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define SWAP(a,b) {swap=(a);(a)=(b);(b)=swap;}

#define NR_END 1
#define FREE_ARG char*
int *ivector(long nl, long nh)
// allocate an int vector with subscript range v[nl..nh]
{
	int *v;
	v=(int *)malloc((size_t)((nh-nl+1+NR_END)*sizeof(int)));
	if (!v)
	{
		printf("ERROR:allocation failure in ivector()\n");
		exit(1);
	}
	return v-nl+NR_END;
}
void free_ivector(int *v, long nl, long nh)
// free an int vector allocated with ivector()
{
	free((FREE_ARG)(v+nl-NR_END));
}
float *vector(long nl, long nh)
// allocate a float vector with subscript range v[nl..nh]
{
	float *v;
	v=(float*)malloc((size_t)((nh-nl+1+NR_END)*sizeof(float)));
	if (!v)
	{
		printf("ERROR:allocation failure in vector()\n");
		exit(1);
	}
	return v-nl+NR_END;
}
void free_vector(float *v, long nl, long nh)
// free a float vector allocated with vector()
{
	free((FREE_ARG)(v+nl-NR_END));
}
float **matrix(long nrl, long nrh, long ncl, long nch)
// allocate a float matrix with subscript range m[nrl..nrh][ncl..nch]
{
	long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
	float **m;
	// allocate pointers to rows
	m=(float **)malloc((size_t)((nrow+NR_END)*sizeof(float*)));
	if (!m)
	{
		printf("allocation failure 1 in matrix()\n");
		exit(1);
	}
	m += NR_END;
	m -= nrl;
	
	// allocate rows and set pointers to them
	m[nrl]=(float *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(float)));
	if (!m[nrl])
	{
		printf("allocation failure 2 in matrix()\n");
		exit(1);
	}
	m[nrl] += NR_END;
	m[nrl] -= ncl;
	
	for(i=nrl+1;i<=nrh;i++) m[i]=m[i-1]+ncol;
	
	// return pointer to array of pointers to rows
	return m;
}
void free_matrix(float **m, long nrl, long nrh, long ncl, long nch)
// free a float matrix allocated by matrix()
{
	free((FREE_ARG) (m[nrl]+ncl-NR_END));
	free((FREE_ARG) (m+nrl-NR_END));
}

void covsrt(float **covar, int ma, int ia[], int mfit)
// Expand in storage the covariance matrix covar, so as to take into account parameters that are
// being held fixed. (For the latter, return zero covariances.)
{
	int i,j,k;
	float swap;
	for (i=mfit+1;i<=ma;i++)
		for (j=1;j<=i;j++) covar[i][j]=covar[j][i]=0.0;
	k=mfit;
	for (j=ma;j>=1;j--)
	{
		if (ia[j])
		{
			for (i=1;i<=ma;i++) SWAP(covar[i][k],covar[i][j])
				for (i=1;i<=ma;i++) SWAP(covar[k][i],covar[j][i])
					k--;
		}
	}
}

void gaussj(float **a, int n, float **b, int m)
// Linear equation solution by Gauss-Jordan elimination, equation (2.1.1) above. a[1..n][1..n]
// is the input matrix. b[1..n][1..m] is input containing the m right-hand side vectors. On
// output, a is replaced by its matrix inverse, and b is replaced by the corresponding set of solution vectors.
{
	int *indxc,*indxr,*ipiv;
	int i,icol,irow,j,k,l,ll;
	float big,dum,pivinv,swap;
	
	indxc=ivector(1,n);			// The integer arrays ipiv, indxr, and indxc are used for bookkeeping on the pivoting.
	indxr=ivector(1,n);
	ipiv=ivector(1,n);
	for (j=1;j<=n;j++) ipiv[j]=0;
	for (i=1;i<=n;i++)			// This is the main loop over the columns to be reduced.
	{
		big=0.0;
		for (j=1;j<=n;j++)		// This is the outer loop of the search for a pivot element.
			if (ipiv[j] != 1)
				for (k=1;k<=n;k++)
				{
					if (ipiv[k] == 0)
					{
						if (fabs(a[j][k]) >= big)
						{
							big=fabs(a[j][k]);
							irow=j;
							icol=k;
						}
					}
					else
						if(ipiv[k] > 1)
						{
							printf("ERROR: gaussj, Singular Matrix-1\n");
							exit(1);
						}
				}
		++(ipiv[icol]);
		// We now have the pivot element, so we interchange rows, if needed, to put the pivot
		// element on the diagonal. The columns are not physically interchanged, only relabeled:
		// indxc[i], the column of the ith pivot element, is the ith column that is reduced, while
		// indxr[i] is the row in which that pivot element was originally located. If indxr[i] !=
		// indxc[i] there is an implied column interchange. With this form of bookkeeping, the
		// solution b’s will end up in the correct order, and the inverse matrix will be scrambled by columns.
		if (irow != icol)
		{
			for (l=1;l<=n;l++) SWAP(a[irow][l],a[icol][l])
				for (l=1;l<=m;l++) SWAP(b[irow][l],b[icol][l])
					}
		indxr[i]=irow;			// We are now ready to divide the pivot row by the
		indxc[i]=icol;			// pivot element, located at irow and icol.
		if (a[icol][icol] == 0.0)
		{
			printf("ERROR: gaussj, Singular Matrix-2\n");
			exit(1);
		}
		pivinv=1.0/a[icol][icol];
		a[icol][icol]=1.0;
		for (l=1;l<=n;l++) a[icol][l] *= pivinv;
		for (l=1;l<=m;l++) b[icol][l] *= pivinv;
		for (ll=1;ll<=n;ll++) if (ll != icol)	// Next, we reduce the rows...
			if(ll!=icol)
			{									// ...except for the pivot one, of course.
				dum=a[ll][icol];
				a[ll][icol]=0.0;
				for (l=1;l<=n;l++) a[ll][l] -= a[icol][l]*dum;
				for (l=1;l<=m;l++) b[ll][l] -= b[icol][l]*dum;
			}
	}
	// This is the end of the main loop over columns of the reduction. It only remains to unscram-
	// ble the solution in view of the column interchanges. We do this by interchanging pairs of
	// columns in the reverse order that the permutation was built up.
	for (l=n;l>=1;l--)
	{
		if (indxr[l] != indxc[l])
			for (k=1;k<=n;k++)
				SWAP(a[k][indxr[l]],a[k][indxc[l]]);
	}						// And we are done.
	free_ivector(ipiv,1,n);
	free_ivector(indxr,1,n);
	free_ivector(indxc,1,n);
}

void mrqcof(float x[], float y[], float sig[], int ndata, float a[], int ia[],
			int ma, float **alpha, float beta[], float *chisq,
			void (*funcs)(float, float [], float *, float [], int))
// Used by mrqmin to evaluate the linearized fitting matrix alpha, and vector beta as in (15.5.8),
// and calculate X2.
{
	int i,j,k,l,m,mfit=0;
	float ymod,wt,sig2i,dy,*dyda;
	
	dyda=vector(1,ma);
	for (j=1;j<=ma;j++)
		if (ia[j]) mfit++;
	for (j=1;j<=mfit;j++)
	{								// Initialize (symmetric) alpha, beta.
		for (k=1;k<=j;k++) alpha[j][k]=0.0;
		beta[j]=0.0;
	}
	*chisq=0.0;
	for (i=1;i<=ndata;i++)
	{								// Summation loop over all data.
		(*funcs)(x[i],a,&ymod,dyda,ma);
		sig2i=1.0/(sig[i]*sig[i]);
		dy=y[i]-ymod;
		for (j=0,l=1;l<=ma;l++)
		{
			if (ia[l])
			{
				wt=dyda[l]*sig2i;
				for (j++,k=0,m=1;m<=l;m++)
					if (ia[m]) alpha[j][++k] += wt*dyda[m];
				beta[j] += dy*wt;
			}
		}
		*chisq += dy*dy*sig2i;		// And find X2.
	}
	for(j=2;j<=mfit;j++)			// Fill in the symmetric side.
		for (k=1;k<j;k++) alpha[k][j]=alpha[j][k];
	free_vector(dyda,1,ma);
}

void mrqmin(float x[], float y[], float sig[], int ndata, float a[], int ia[],
			int ma, float **covar, float **alpha, float *chisq,
			void (*funcs)(float, float [], float *, float [], int), float *alamda)
// Levenberg-Marquardt method, attempting to reduce the value χ2 of a fit between a set of data
// points x[1..ndata], y[1..ndata] with individual standard deviations sig[1..ndata],
// and a nonlinear function dependent on ma coefficients a[1..ma]. The input array ia[1..ma]
// indicates by nonzero entries those components of a that should be fitted for, and by zero
// entries those components that should be held fixed at their input values. The program re-
// turns current best-fit values for the parameters a[1..ma], and χ2 = chisq. The arrays
// covar[1..ma][1..ma], alpha[1..ma][1..ma] are used as working space during most
// iterations. Supply a routine funcs(x,a,yfit,dyda,ma) that evaluates the fitting function
// yfit, and its derivatives dyda[1..ma] with respect to the fitting parameters a at x. On
// the first call provide an initial guess for the parameters a, and set alamda<0 for initialization
// (which then sets alamda=.001). If a step succeeds chisq becomes smaller and alamda de-
// creases by a factor of 10. If a step fails alamda grows by a factor of 10. You must call this
// routine repeatedly until convergence is achieved. Then, make one final call with alamda=0, so
// that covar[1..ma][1..ma] returns the covariance matrix, and alpha the curvature matrix.
// (Parameters held fixed will return zero covariances.)
{
	int j,k,l;
	static int mfit;
	static float ochisq,*atry,*beta,*da,**oneda;
	
	if(*alamda < 0.0)					// Initialization.
	{
		atry=vector(1,ma);
		beta=vector(1,ma);
		da=vector(1,ma);
		for(mfit=0,j=1;j<=ma;j++)
			if (ia[j]) mfit++;
		oneda=matrix(1,mfit,1,1);
		*alamda=0.001;
		mrqcof(x,y,sig,ndata,a,ia,ma,alpha,beta,chisq,funcs);
		ochisq=(*chisq);
		for(j=1;j<=ma;j++) atry[j]=a[j];
	}
	for (j=1;j<=mfit;j++)				// Alter linearized fitting matrix, by augmenting di-
	{									// agonal elements.
		for (k=1;k<=mfit;k++) covar[j][k]=alpha[j][k];
		covar[j][j]=alpha[j][j]*(1.0+(*alamda));
		oneda[j][1]=beta[j];
	}
	gaussj(covar,mfit,oneda,1);			// Matrix solution.
	for (j=1;j<=mfit;j++) da[j]=oneda[j][1];
	if (*alamda == 0.0)					// Once converged, evaluate covariance matrix.
	{
		covsrt(covar,ma,ia,mfit);
		covsrt(alpha,ma,ia,mfit);		// Spread out alpha to its full size too.
		free_matrix(oneda,1,mfit,1,1);
		free_vector(da,1,ma);
		free_vector(beta,1,ma);
		free_vector(atry,1,ma);
		return;
	}
	for (j=0,l=1;l<=ma;l++)				// Did the trial succeed?
		if (ia[l]) atry[l]=a[l]+da[++j];
	mrqcof(x,y,sig,ndata,atry,ia,ma,covar,da,chisq,funcs);
	if (*chisq < ochisq)
	{									// Success, accept the new solution.
		*alamda *= 0.1;
		ochisq=(*chisq);
		for (j=1;j<=mfit;j++)
		{
			for (k=1;k<=mfit;k++) alpha[j][k]=covar[j][k];
			beta[j]=da[j];
		}
		for (l=1;l<=ma;l++) a[l]=atry[l];
	}
	else
	{									// Failure, increase alamda and return.
		*alamda *= 10.0;
		*chisq=ochisq;
	}
}
void vonbertalanfy(float x, float a[], float *y, float dyda[], int na)
{
	float ex=exp(-x/a[2]);
	*y=a[1]*(1-ex);
	dyda[1]=1-ex;
	dyda[2]=-a[1]*x*ex/pow(a[2],2);
}
int main(int argc, char *argv[])
{
	int		ndata=8;
	int		ma=2;
	float	x[9]={0,91,181,288,378,455,552,714,918};
	float	y[9]={0,688251,873312,1015071,1078053,1115652,1170489,1214700,1261218};
	float	sig[9]={0,1,1,1,1,1,1,1,1};
	float	a[3]={0,1000000,500};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int	i;
	
	for(i=1;i<=8;i++)
		x[i]+=270;

	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,vonbertalanfy,&alamda);
	printf("alamda=%f\n",alamda);

	for(i=0;i<50;i++)
	{
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,vonbertalanfy,&alamda);
		printf("X2=%f\n",chisq);
	}
	
	printf("b0=%f, b1=%f, X2=%f\n",a[1],a[2],chisq);
	
	return 0;
}
