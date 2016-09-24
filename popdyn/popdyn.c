#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int		*sub;
float	*t;
float	*v;
int		nrows;
int		nsubs;

#define SWAP(a,b) {swap=(a);(a)=(b);(b)=swap;}
#define MIN(a,b) (((a)<(b))?(a):(b))
#pragma mark -

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
							//exit(1);
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
			//exit(1);
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
	//printf("D=");
	for (i=1;i<=ndata;i++)
	{								// Summation loop over all data.
		(*funcs)(x[i],a,&ymod,dyda,ma);
		sig2i=1.0/(sig[i]*sig[i]);
		dy=y[i]-ymod;
		//printf("(%.3g,%.3g) ",y[i],ymod);
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
	
	//printf("mrqmin n=%i\n",ndata);
	//for(j=1;j<=ndata;j++) printf("%g\n",y[j]);

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
#pragma mark -
float amotry(float *p, float y[], float psum[], int ndim,float (*funk)(float []), int ihi, float fac)
// Extrapolates by a factor fac through the face of the simplex across from the high point, tries
// it, and replaces the high point if the new point is better.
{
	int		j;
	float	fac1,fac2,ytry,*ptry;

	ptry=(float*)calloc(ndim+1,sizeof(float));
	fac1=(1.0-fac)/ndim;
	fac2=fac1-fac;
	for (j=1;j<=ndim;j++)
		ptry[j]=psum[j]*fac1-p[ndim*(ihi-1)+j]*fac2;
	ytry=(*funk)(ptry);		// Evaluate the function at the trial point.
	if (ytry < y[ihi])
	{
		// If it's better than the highest, then replace the highest.
		y[ihi]=ytry;
		for (j=1;j<=ndim;j++)
		{
			psum[j] += ptry[j]-p[ndim*(ihi-1)+j];
			p[ndim*(ihi-1)+j]=ptry[j];
		}
	}
	free(ptry);
	return ytry;
}
#define TINY 1.0e-10	// A small number.
#define NMAX 500000  	//Maximum allowed number of function evaluations.
#define GET_PSUM \
					for (j=1;j<=ndim;j++) {\
						for (sum=0.0,i=1;i<=mpts;i++) sum += p[ndim*(i-1)+j];\
						psum[j]=sum;}
void amoeba(float *p, float y[], int ndim, float ftol,float (*funk)(float []),int *nfunk)
// Multidimensional minimization of the function funk(x) where x[1..ndim] is a vector in ndim
// dimensions, by the downhill simplex method of Nelder and Mead. The matrix p[1..ndim+1]
// [1..ndim] is input. Its ndim+1 rows are ndim-dimensional vectors which are the vertices of
// the starting simplex. Also input is the vector y[1..ndim+1], whose components must be pre-
// initialized to the values of funk evaluated at the ndim+1 vertices (rows) of p; and ftol the
// fractional convergence tolerance to be achieved in the function value (n.b.!). On output, p and
// y will have been reset to ndim+1 new points all within ftol of a minimum function value, and
// nfunk gives the number of function evaluations taken.
{
	int 	i,ihi,ilo,inhi,j,mpts=ndim+1;
	float	rtol,sum,swap,ysave,ytry,*psum;

	psum=(float*)calloc(ndim+1,sizeof(float));
	*nfunk=0;
	GET_PSUM
	for (;;)
	{
		ilo=1;
		// First we must determine which point is the highest (worst), next-highest, and lowest
		// (best), by looping over the points in the simplex.
		ihi = y[1]>y[2] ? (inhi=2,1) : (inhi=1,2);
		for (i=1;i<=mpts;i++)
		{
			if (y[i] <= y[ilo])
				ilo=i;
			if (y[i] > y[ihi])
			{
				inhi=ihi;
				ihi=i;
			}
			else if (y[i] > y[inhi] && i != ihi)
				inhi=i;
		}
		rtol=2.0*fabs(y[ihi]-y[ilo])/(fabs(y[ihi])+fabs(y[ilo])+TINY);
		// Compute the fractional range from highest to lowest and return if satisfactory.
		if (rtol < ftol)
		{
			// If returning, put best point and value in slot 1.
			SWAP(y[1],y[ilo])
			for (i=1;i<=ndim;i++) SWAP(p[ndim*(1-1)+i],p[ndim*(ilo-1)+i])
			break;
		}
		if (*nfunk >= NMAX){ break;};
		*nfunk += 2;
		// Begin a new iteration. First extrapolate by a factor -1 through the face of the simplex
		// across from the high point, i.e., reflect the simplex from the high point.
		ytry=amotry(p,y,psum,ndim,funk,ihi,-1.0);
		if (ytry <= y[ilo])
			// Gives a result better than the best point, so try an additional extrapolation by a
			// factor 2.
			ytry=amotry(p,y,psum,ndim,funk,ihi,2.0);
		else if (ytry >= y[inhi])
		{
			// The reflected point is worse than the second-highest, so look for an intermediate
			// lower point, i.e., do a one-dimensional contraction.
			ysave=y[ihi];
			ytry=amotry(p,y,psum,ndim,funk,ihi,0.5);
			if (ytry >= ysave)
			{
				// Can't seem to get rid of that high point. Better
				// contract around the lowest (best) point.
				for (i=1;i<=mpts;i++)
				{
					if (i != ilo)
					{
						for (j=1;j<=ndim;j++)
							p[ndim*(i-1)+j]=psum[j]=0.5*(p[ndim*(i-1)+j]+p[ndim*(ilo-1-1)+j]);
						y[i]=(*funk)(psum);
					}
				}
				*nfunk += ndim;		// Keep track of function evaluations.
				GET_PSUM			// Recompute psum.
			}
		}
		else
			--(*nfunk);				// Correct the evaluation count.
	}								// Go back for the test of doneness and the next
	free(psum);
}
#pragma mark -
float map_vb_one_sqrerr(float A, float T, int subIndex)
{
	int		i,n;
	float	V,dy,err;
	
	err=0;
	n=0;
	//printf("DD=");
	for(i=0;i<nrows;i++)
	if(sub[i]==subIndex)
	{
		V=A*(1-exp(-t[i]/T));
		dy=v[i]-V;
		//printf("(%.3g,%.3g) ",v[i],A*(1-exp(-t[i]/T)));
		err+=dy*dy;
		n++;
	}
	//printf("map_vb_one_sqrerr n=%i\n",n);
	return err/(float)n;
}
void write_tiff(char *path, float *m, int W, int H)
{
	FILE	*f;
	int		i,n,S;
	unsigned char	hdr[]={	0x4d,0x4d,
							0x00,0x2a,
							0x00,0x00,0x00,0x08,
							0x00,0x0d,														// declare 13 fields
							0x01,0xfe, 0x00,0x04, 0x00,0x00,0x00,0x01, 0x00,0x00,0x00,0x00,
							0x01,0x00, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x40,0x00,0x00,	// image width
							0x01,0x01, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x20,0x00,0x00,	// image length
							0x01,0x02, 0x00,0x03, 0x00,0x00,0x00,0x03, 0x00,0x00,0x00,0xaa,	// bits per sample [addr: aa]
							0x01,0x03, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x01,0x00,0x00,	// compression
							0x01,0x06, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x02,0x00,0x00,	// photometric interpretation
							0x01,0x11, 0x00,0x04, 0x00,0x00,0x00,0x01, 0x00,0x00,0x00,0xc0,	// strip offsets [addr:0xc0]
							0x01,0x15, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x03,0x00,0x00,	// samples per pixel
							0x01,0x16, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x20,0x00,0x00,	// rows per strip
							0x01,0x17, 0x00,0x04, 0x00,0x00,0x00,0x01, 0x00,0x00,0x18,0x00,	// strip byte counts
							0x01,0x1a, 0x00,0x05, 0x00,0x00,0x00,0x01, 0x00,0x00,0x00,0xb0,	// x resolution [addr: b0]
							0x01,0x1b, 0x00,0x05, 0x00,0x00,0x00,0x01, 0x00,0x00,0x00,0xb8,	// y resolution [addr: b8]
							0x01,0x28, 0x00,0x03, 0x00,0x00,0x00,0x01, 0x00,0x02,0x00,0x00,	// resolution unit
							0x00,0x00,0x00,0x00,
							0x00,0x08, 0x00,0x08, 0x00,0x08,								// [addr 0xAA] bits per sample: 8,8,8
							0x00,0x0a,0xfc,0x80, 0x00,0x00,0x27,0x10,						// [addr 0xB0] x resolution 72
							0x00,0x0a,0xfc,0x80, 0x00,0x00,0x27,0x10,						// [addr 0xB8] y resolution 72
							};
	// image width
	hdr[31]=((unsigned char *)&W)[0];
	hdr[30]=((unsigned char *)&W)[1];
	hdr[33]=((unsigned char *)&W)[2];
	hdr[32]=((unsigned char *)&W)[3];
	
	// image length
	hdr[43]=((unsigned char *)&H)[0];
	hdr[42]=((unsigned char *)&H)[1];
	hdr[45]=((unsigned char *)&H)[2];
	hdr[44]=((unsigned char *)&H)[3];
	
	// rows per strip = image length
	hdr[115]=((unsigned char *)&H)[0];
	hdr[114]=((unsigned char *)&H)[1];
	hdr[117]=((unsigned char *)&H)[2];
	hdr[116]=((unsigned char *)&H)[3];
	
	// strip byte counts = image length * image width * 3
	S=W*H*3;
	hdr[129]=((unsigned char *)&S)[0];
	hdr[128]=((unsigned char *)&S)[1];
	hdr[127]=((unsigned char *)&S)[2];
	hdr[126]=((unsigned char *)&S)[3];
	
	
	f=fopen(path,"w");
	n=sizeof(hdr);
	for(i=0;i<n;i++)
		fputc(hdr[i],f);
	for(i=0;i<W*H;i++)
	{
		fputc((int)(0xff*m[i]),f);
		if(m[i]<1/10.0)
			fputc((int)(0xff*m[i]*10),f);
		else
			fputc(0,f);
		if(m[i]<1/100.0)
			fputc((int)(0xff*m[i]*100),f);
		else
			fputc(0,f);
	}
	fclose(f);
	
}
void map_vb_all_pic(float A0, float A1, float T0, float T1, int nA, int nT,char *base)
{
	int		i,a,t,flag=0;
	float	A,T;
	float	sqrerr,min,max,minA,minT;
	float	*chi2;
	char	path[1024];
	
	chi2=(float*)calloc((nA+1)*(nT+1),sizeof(float));
	
	for(i=1;i<=nsubs;i++)
	{
		flag=0;
		for(a=0;a<=nA;a++)
		{
			A=A0+a*(A1-A0)/(float)nA;
			for(t=0;t<=nT;t++)
			{
				T=T0+t*(T1-T0)/(float)nT;
	
				sqrerr=map_vb_one_sqrerr(A,T,i);
				chi2[t*nA+a]=sqrerr;
				
				if(flag==0)
				{
					min=max=sqrerr;
					minA=A;
					minT=T;
					flag=1;
				}
				if(sqrerr<min)
				{
					min=sqrerr;
					minA=A;
					minT=T;
				}
				if(sqrerr>max)
					max=sqrerr;
			}
		}
		//min=0;
		max=100000;
		for(t=0;t<=nT;t++)
		for(a=0;a<=nA;a++)
			chi2[t*nA+a]=MIN(1,(chi2[t*nA+a]-min)/(max-min));
		chi2[((int)((minT-T0)/(T1-T0)*nT))*nA+ (int)((minA-A0)/(A1-A0)*nA) ]=1;
		sprintf(path,"%s_%i_%.1f_%.1f.tif",base,i,min,max);
		write_tiff(path,chi2,nA,nT);
	}
}
void map_vb_all_txt(float A0, float A1, float T0, float T1, int nA, int nT, char *base)
{
	int		i,flag=0;
	float	A,T;
	float	sqrerr,min,minA,minT;
	float	*chi2;
	FILE	*f;
	char	path[1024];
	
	chi2=(float*)calloc(((int)(A1-A0))*((int)(A1-A0)),sizeof(float));
	
	sprintf(path,"%s.txt",base);
	f=fopen(path,"w");
	fprintf(f,"X=[");
	for(A=A0;A<=A1;A+=(A1-A0)/(float)nA)
		fprintf(f,"%g ",A);
	fprintf(f,"];\n");
	fprintf(f,"Y=[");
	for(T=T0;T<=T1;T+=(T1-T0)/(float)nT)
		fprintf(f,"%g ",T);
	fprintf(f,"];\n");
	fprintf(f,"\n");
	
	for(i=1;i<=nsubs;i++)
	{
		fprintf(f,"sub%i=[",i);
		min=0;
		for(A=A0;A<=A1;A+=(A1-A0)/(float)nA)
		{
			for(T=T0;T<=T1;T+=(T1-T0)/(float)nT)
			{
				sqrerr=map_vb_one_sqrerr(A,T,i);
				fprintf(f,"%g ",sqrerr);
				
				if(flag==0)
				{
					min=sqrerr;
					minA=A;
					minT=T;
					flag=1;
				}
				if(sqrerr<min)
				{
					min=sqrerr;
					minA=A;
					minT=T;
				}
			}
			fprintf(f,"\n");
		}
		fprintf(f,"];\n");
		fprintf(f,"min%i=%g, A%i=%g, T%i=%g\n",i,min,i,minA,i,minT);
	}
	fclose(f);
}
#pragma mark -
float avrg_vb_percent_error(int subIndex, float A, float T)
{
	int		i,n;
	float	apr,V;
	
	apr=0;
	n=0;
	for(i=0;i<nrows;i++)
	if(sub[i]==subIndex && v[i]>0)
	{
		V=A*(1-exp(-t[i]/T));
		apr+=fabs(V-v[i])/v[i];
		n++;
	}
	apr/=(float)n;
	return 100*apr;
}
float avrg_g_percent_error(int subIndex, float A, float B, float C)
{
	int		i,n;
	float	apr,V;
	
	apr=0;
	n=0;
	for(i=0;i<nrows;i++)
	if(sub[i]==subIndex && v[i]>0)
	{
		V=A*exp(-B*exp(-t[i]/C));
		apr+=fabs(V-v[i])/v[i];
		n++;
	}
	apr/=(float)n;
	return 100*apr;
}
#pragma mark -
void g(float x, float a[], float *y, float dyda[], int na)
{
	// a[1]: asymptote
	// a[2]: time displacement
	// a[3]: growth rate (time scaling)
	float	ex=exp(-a[2]*exp(-x/a[3]));
	*y=a[1]*ex;
	dyda[1]=ex;
	dyda[2]=-a[1]*ex*exp(-x/a[3]);
	dyda[3]=-a[1]*a[2]*x/pow(a[3],2)*ex*exp(-x/a[3]);
}
void fit_g_one(float *x, float *y, int ndata, float *A, float *B, float *C)
{
	int		ma=3;
	float	*sig;
	float	a[4]={0,1000.000,10,100};
	int		ia[4]={0,1,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
		sig[i+1]=1;

	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,g,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,g,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	free(sig);
	
	*A=a[1];
	*B=a[2];
	*C=a[3];
}
void fit_g_all(float *B, float *alpha, float *beta)
{
	int		i,j,i0,n;
	float	*A0,*B0,*C0;
	float	avrgA,avrgB,avrgC,x0,x1,p[2],m[2];
	float	err,totalErr;
	
	A0=(float*)calloc(nsubs,sizeof(float));
	B0=(float*)calloc(nsubs,sizeof(float));
	C0=(float*)calloc(nsubs,sizeof(float));
	
	// Fit each subject
	n=0;
	i0=0;
	printf("sub A B C %%err\n");
	totalErr=0;
	for(i=0;i<nrows;i++)
	if(sub[i]!=sub[(i+1)%nrows])
	{
		fit_g_one(&(t[i0-1]),&(v[i0-1]),i-i0, &(A0[n]),&(B0[n]),&(C0[n]));
		err=avrg_g_percent_error(sub[i],A0[n],B0[n],C0[n]);
		totalErr+=err;
		printf("%i %g %g %g %.2g\n",sub[i],A0[n],B0[n],C0[n],err);
		i0=i;
		n++;
	}
	printf("Total error: %.2g%%\n\n",totalErr/(float)nsubs);

	// Guess B from average(B), alpha and beta from the 1st eigenvector of [Ai Ci], i=0, ..., (nsubs-1)
	// (find 1st eigenvector iteratively)
	avrgA=0;
	avrgB=0;
	avrgC=0;
	for(i=0;i<nsubs;i++)
	{
		avrgA+=A0[i];
		avrgB+=B0[i];
		avrgC+=C0[i];
	}
	avrgA/=(float)nsubs;
	avrgB/=(float)nsubs;
	avrgC/=(float)nsubs;
	p[0]=rand()%100;
	p[1]=rand()%100;
	for(i=0;i<10;i++)
	{
		m[0]=m[1]=0;
		for(j=0;j<nsubs;j++)
		{
			x0=(A0[j]-avrgA);
			x1=(C0[j]-avrgC);
			m[0]+=m[0]+(x0*p[0]+x1*p[1])*x0;
			m[1]+=m[1]+(x0*p[0]+x1*p[1])*x1;
		}
		p[0]=m[0]/sqrt(m[0]*m[0]+m[1]*m[1]);
		p[1]=m[1]/sqrt(m[0]*m[0]+m[1]*m[1]);
	}
	*B=avrgB;
	*alpha=avrgC-(p[1]/p[0])*avrgA;
	*beta=p[1]/p[0];
	
	printf("Parameter guess for B and C=f(A)\n");
	printf("avrgA=%g, avrgB=%g, avrgC=%g, p[]=[%g %g]\n",avrgA,avrgB,avrgC,p[0],p[1]);
	printf("Alpha guess: %g\n",*alpha);
	printf("Beta guess: %g\n",*beta);
	printf("\n");
}
#pragma mark -
float fit_g_pop_sqrerr(float *param)
{
	int				i;
	float			A,B,alpha,beta,V,err;
	
	B=param[1];
	alpha=param[2];
	beta=param[3];
	
	err=0;
	for(i=0;i<nrows;i++)
	{
		A=param[3+sub[i]];
		V=A*exp(-B*exp(-t[i]/(alpha+beta*A)));
		err+=pow(v[i]-V,2);
	}
	return err;
}
void fit_g_pop(float B, float alpha, float beta)
// Fit Gompertz model: Vi(t)=Ai*exp(-B*exp(-t/(alpha+beta*Ai)))
{
	int		i,j;
	int		ndim;
	int		nfunk;
	float	ftol=0.00001;
	float	*p,*f;
	float	A,C,err,totalErr;
	
	// ndim is the number of dimensions
	ndim=nsubs+3;	// Ai, B, alpha, beta, avec i=1...nsubs
	
	// The simplex p has ndim+1 vertices in ndim space (vector starts at index 1)
	p=(float*)calloc(ndim*(ndim+1)+1,sizeof(float));
	
	// Values at the base point
	p[1]=B;			// guess for B
	p[2]=alpha;		// guess alpha
	p[3]=beta;		// guess beta
	for(i=0;i<nrows;i++)	// guess for Ai: the last value in the series for subject i
		if(sub[i]!=sub[(i+1)%nrows])
			p[4+(sub[i]-1)]=v[i];
	
	// Extend the simplex from the base vertex, 10% in each dimension
	for(i=2;i<=ndim+1;i++)
		for(j=1;j<=ndim;j++)
			p[ndim*(i-1)+j]=p[j]*(1+0.1*(j+1==i));
	
	// Evaluation of the error function at each of the ndim+1 simplex vertices
	f=(float*)calloc(ndim*(ndim+1)+1,sizeof(float));
	for(i=1;i<=ndim+1;i++)
		f[i]=fit_g_pop_sqrerr(&p[(ndim+1)*(i-1)]);
	
	amoeba(	p,					// ndimensional coordinates of the starting simplex
		   f,					// value of funk at the vertices of the simplex
		   ndim,				// number of dimensions
		   ftol,				// tolerance
		   fit_g_pop_sqrerr,	// funk: function to minimise
		   &nfunk				// number of function evaluations
		   );
		   
	printf("B: %g\n",p[1]);
	printf("alpha: %g\n",p[2]);
	printf("beta: %g\n",p[3]);
	printf("sub A B C %%err\n");
	totalErr=0;
	for(i=4;i<ndim+1;i++)
	{
		A=p[i];
		B=p[1];
		C=p[2]+p[3]*p[i];
		err=avrg_g_percent_error(i-3,A,B,C);
		totalErr+=err;
		printf("%i %g %g %g %.2g\n",i-3,A,B,C,err);
	}
	printf("Total error: %.2g%%\n",totalErr/(float)nsubs);

	printf("\n");
}
#pragma mark -
void vb(float x, float a[], float *y, float dyda[], int na)
{
	float ex=exp(-x/a[2]);
	*y=a[1]*(1-ex);
	dyda[1]=1-ex;
	dyda[2]=-a[1]*x*ex/pow(a[2],2);
}
float fit_vb_one(float *x, float *y, int ndata, float *A, float *T)
{
	int		ma=2;
	float	*sig;
	float	a[3]={0,1212.000,374.4};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
		sig[i+1]=1;

	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,vb,&alamda);
	for(i=0;i<200;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,vb,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	free(sig);
	*A=a[1];
	*T=a[2];
	
	return chisq;
}
void fit_vb_all(float *alpha, float *beta)
{
	int		i,j,i0,n;
	float	*A0,*T0;
	float	avrgA,avrgT,x0,x1,p[2],m[2];
	float	err,totalErr,chi2;
	
	A0=(float*)calloc(nsubs,sizeof(float));
	T0=(float*)calloc(nsubs,sizeof(float));
	
	// Fit each subject
	n=0;
	i0=-1;
	printf("sub n A T chi2 x %%err\n");
	totalErr=0;
	for(i=0;i<nrows;i++)
	if(sub[i]!=sub[(i+1)%nrows])
	{
		chi2=fit_vb_one(&(t[i0]),&(v[i0]),i-i0, &(A0[n]),&(T0[n]));
		err=avrg_vb_percent_error(sub[i],A0[n],T0[n]);
		totalErr+=err;
		printf("%i %i %g %g %g %.2g\n",sub[i],i-i0,A0[n],T0[n],chi2,err);
		i0=i;
		n++;
	}
	printf("Total error: %.2g%%\n\n",totalErr/(float)nsubs);

	// Guess alpha and beta from the 1st eigenvector of [Ai Ti], i=0, ..., (nsubs-1)
	// (find 1st eigenvector iteratively)
	avrgA=0;
	avrgT=0;
	for(i=0;i<nsubs;i++)
	{
		avrgA+=A0[i];
		avrgT+=T0[i];
	}
	avrgA/=(float)nsubs;
	avrgT/=(float)nsubs;
	p[0]=rand()%100;
	p[1]=rand()%100;
	for(i=0;i<10;i++)
	{
		m[0]=m[1]=0;
		for(j=0;j<nsubs;j++)
		{
			x0=(A0[j]-avrgA);
			x1=(T0[j]-avrgT);
			m[0]+=m[0]+(x0*p[0]+x1*p[1])*x0;
			m[1]+=m[1]+(x0*p[0]+x1*p[1])*x1;
		}
		p[0]=m[0]/sqrt(m[0]*m[0]+m[1]*m[1]);
		p[1]=m[1]/sqrt(m[0]*m[0]+m[1]*m[1]);
	}
	*alpha=avrgT-(p[1]/p[0])*avrgA;
	*beta=p[1]/p[0];
	
	printf("Parameter guess for T=f(A)\n");
	printf("avrgA=%g, avrgT=%g, p[]=[%g %g]\n",avrgA,avrgT,p[0],p[1]);
	printf("Alpha guess: %g\n",*alpha);
	printf("Beta guess: %g\n",*beta);
	printf("\n");
}
#pragma mark -
float fit_vb_pop_sqrerr(float *param)
{
	int				i;
	float			A,alpha,beta,V,err;
	
	alpha=param[1];
	beta=param[2];
	
	err=0;
	for(i=0;i<nrows;i++)
	{
		A=param[2+sub[i]];
		V=A*(1-exp(-t[i]/(alpha+beta*A)));
		err+=pow(v[i]-V,2);
	}
	return err;
}
void fit_vb_pop(float alpha, float beta)
// Fit Vi(t)=Ai*(1-exp(-t/(alpha+beta*Ai)))
{
	int		i,j;
	int		ndim;
	int		nfunk;
	float	ftol=0.00001;
	float	*p,*f;
	float	A,T,err,totalErr;
	
	// ndim is the number of dimensions
	ndim=nsubs+2;	// Ai, alpha, beta, avec i=1...nsubs
	
	// The simplex p has ndim+1 vertices in ndim space (vector starts at index 1)
	p=(float*)calloc(ndim*(ndim+1)+1,sizeof(float));
	
	// Values at the base point
	p[1]=alpha;			// guess for B
	p[2]=beta;			// guess C
	for(i=0;i<nrows;i++)	// guess for Ai: the last value in the series for subject i
		if(sub[i]!=sub[(i+1)%nrows])
			p[3+(sub[i]-1)]=v[i];
	
	// Extend the simplex from the base vertex, 10% in each dimension
	for(i=2;i<=ndim+1;i++)
		for(j=1;j<=ndim;j++)
			p[ndim*(i-1)+j]=p[j]*(1+0.1*(j+1==i));
	
	// Evaluation of the error function at each of the ndim+1 simplex vertices
	purposedly introduced error: the dimension of f was ndim*(ndim+1)+1
	when just (ndim+1) should suffice
	
	f=(float*)calloc((ndim+1)+1,sizeof(float));

	This mistake was also introduced purposedly !
	it seems upond lecture that the for loop below
	has to run up to <=ndim+1, and not <ndim+1
	(otherwise, the last vertex in the simplex is not initialised).
	I also changed it, without testing in fit_g_pop.
	Future me: do some testing and delete this text.
	Really, check also the &p. Shouldn't it be ndim*(i-1) as I'm
	writing now instead of the (ndim+1)*(i-1) that was there before?
	Try probably printing the values of f[i] out, and check they
	make sense (I wrote the code for you)

	for(i=1;i<=ndim+1;i++)
	{
		f[i]=fit_vb_pop_sqrerr(&p[ndim*(i-1)]);
		printf("%g\n",f[i]);
	}
	
	amoeba(	p,					// ndimensional coordinates of the starting simplex
		   f,					// value of funk at the vertices of the simplex
		   ndim,				// number of dimensions
		   ftol,				// tolerance
		   fit_vb_pop_sqrerr,	// funk: function to minimise
		   &nfunk				// number of function evaluations
		   );
		   
	printf("B: %g\n",p[1]);
	printf("C: %g\n",p[2]);
	printf("sub A T %%err\n");
	totalErr=0;
	for(i=3;i<ndim+1;i++)
	{
		A=p[i];
		T=p[1]+p[2]*p[i];
		err=avrg_vb_percent_error(i-2,A,T);
		totalErr+=err;
		printf("%i %g %g %.2g\n",i-2,A,T,err);
	}
	printf("Total error: %.2g%%\n",totalErr/(float)nsubs);

	printf("\n");
}
#pragma mark -
void parse(char *file)
{
	FILE	*f;
	char	str[512];
	int		i,n;
	
	nrows=0;
	f=fopen(file,"r");
	while(!feof(f))
	{
		fgets(str,512,f);
		nrows++;
	}
	fclose(f);
	nrows--;
	
	sub=(int*)calloc(nrows,sizeof(int));
	t=(float*)calloc(nrows,sizeof(float));
	v=(float*)calloc(nrows,sizeof(float));

	f=fopen(file,"r");
	for(i=0;i<nrows;i++)
	{
		fgets(str,512,f);
		n=sscanf(str," %i %f %f ",&(sub[i]),&(t[i]),&(v[i]));
		if(n<3)
			break;
		v[i]/=1000.0;
	}
	fclose(f);
	nsubs=sub[nrows-1];
	
	printf("Read %i data points for %i subjects\n\n",nrows,nsubs);
}
int main(int argc, char *argv[])
{
	// argv[1]	data file. Format: #subj time value [everything else in the row is ignored]
	// argv[2]	model={vb,g,mvb}. vb for Von Bertalanffy's growth model, g for Gompertz growth model
	//
	// #sub has consecutive values from 1 to the number of subjects
	// time is >=0
	// value(time) is >=0

	parse(argv[1]);

	if(strcmp(argv[2],"vb")==0)
	{
		float	B;	// guess for B
		float	C;	// guess for C
		
		printf("Individual Von Bertalanffy model fit\n");
		fit_vb_all(&B,&C);
		
		printf("Group Von Bertalanffy model fit\n");
		fit_vb_pop(B,C);
	}
	else
	if(strcmp(argv[2],"g")==0)
	{
		float	B;		// guess for B
		float	alpha;	// guess for alpha
		float	beta;	// guess for beta
		
		printf("Individual Gompertz model fit\n");
		fit_g_all(&B,&alpha,&beta);
		
		printf("Group Gompertz model fit\n");
		fit_g_pop(B,alpha,beta);
	}
	else
	if(strcmp(argv[2],"mvb")==0)
	{
		float	A0,A1;	// min and max limits for A
		float	T0,T1;	// min and max limits for T
		int		nA,nT;
		char	*mode,*base;

		printf("Saving individual fitting landscapes\n");
		A0=atof(argv[3]);
		A1=atof(argv[4]);
		T0=atof(argv[5]);
		T1=atof(argv[6]);
		nA=atoi(argv[7]);
		nT=atoi(argv[8]);
		base=argv[9]; // base name for output files
		mode=argv[10]; // either "txt" or "tif"
		
		if(strcmp(mode,"txt")==0)
			map_vb_all_txt(A0,A1,T0,T1,nA,nT,base);
		if(strcmp(mode,"pic")==0)
			map_vb_all_pic(A0,A1,T0,T1,nA,nT,base);
		
	}
	else
		printf("ERROR: Unknown model %s\n",argv[2]);
	
	return 1;
}