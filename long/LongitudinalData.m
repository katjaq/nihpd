//
//  LongitudinalData.m
//  long
//
//  Created by roberto on 18/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "LongitudinalData.h"


@implementation LongitudinalData
#pragma mark -
-(void)setData:(NSMutableDictionary*)theData
{
	data=theData;
	[self findLimitsForAge];
	[self colorsForSubjects];
}
-(void)setVariableIndex:(int)theVindex
{
	vindex=theVindex;
	[self findLimitsForVariable];
	[self findLimitsForVariableDerivate];
}
-(void)setGroupIndex:(int)theGindex
{
	gindex=theGindex;
}
-(void)setRepresentationIndex:(int)theRindex
{
	rindex=theRindex;
}
-(void)setTransformIndex:(int)theTindex
{
	tindex=theTindex;
}
-(void)colorsForSubjects
{
	int			N,i;
	NSArray		*sub=[data objectForKey:@"Subject"];
	NSString	*s=@"EMPTY";
	NSColor		*color;
	
	if(subcolors)
		[subcolors release];
	subcolors=[NSMutableDictionary new];
	
	N=[sub count];
	for(i=0;i<N;i++)
	{
		if([s isEqualTo:[sub objectAtIndex:i]]==NO)
		{
			color=[NSColor colorWithDeviceRed:rand()/(float)RAND_MAX green:rand()/(float)RAND_MAX blue:rand()/(float)RAND_MAX alpha:1];
			[subcolors setObject:color forKey:[sub objectAtIndex:i]];
		}
		s=[sub objectAtIndex:i];
	}
}
-(void)findLimitsForAge
{
	int				N,i;
	NSArray			*age=[data objectForKey:@"Age"];
	float			x;
	
	tmin=tmax=[[age objectAtIndex:0] floatValue];
	N=[age count];
	for(i=0;i<N;i++)
	{
		x=[[age objectAtIndex:i] floatValue];
		
		if(x<tmin)	tmin=x;
		if(x>tmax)	tmax=x;
	}
}
-(void)findLimitsForVariable
{
	int		N,i;
	NSArray	*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	float	y;
	
	vmin=vmax=[[var objectAtIndex:0] floatValue];
	N=[var count];
	for(i=0;i<N;i++)
	{
		y=[[var objectAtIndex:i] floatValue];
		
		if(y<vmin)	vmin=y;
		if(y>vmax)	vmax=y;
	}
}
-(void)findLimitsForVariableDerivate
{
	int			N,i;
	NSArray		*sub=[data objectForKey:@"Subject"];
	NSArray		*age=[data objectForKey:@"Age"];
	NSArray		*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	NSString	*s=@"EMPTY";
	float		x0,x1,y0,y1,dv;
	
	N=[age count];
	for(i=0;i<N;i++)
	{
		if([s isEqualTo:[sub objectAtIndex:i]]==NO)
		{
			x0=[[age objectAtIndex:i] floatValue];
			y0=[[var objectAtIndex:i] floatValue];
		}
		else
		{
			x1=[[age objectAtIndex:i] floatValue];
			y1=[[var objectAtIndex:i] floatValue];
			
			dv=(y1-y0)/(x1-x0);
			if(dv<dvmin)	dvmin=dv;
			if(dv>dvmax)	dvmax=dv;
			
			x0=x1;
			y0=y1;
		}
		s=[sub objectAtIndex:i];
	}
}
-(void)drawData
{
	int				i,j,N;
	float			x,y;
	NSArray			*sub=[data objectForKey:@"Subject"];
	NSArray			*age=[data objectForKey:@"Age"];
	NSArray			*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	NSMutableArray	*raw=[NSMutableArray new];
	
	[self findLimitsForAge];
	[self findLimitsForVariable];
	
	j=-1;
	N=[age count];
	for(i=0;i<N;i++)
	{
		x=[[age objectAtIndex:i] floatValue];
		y=[[var objectAtIndex:i] floatValue];
		[raw addObject:NSStringFromPoint((NSPoint){x,y})];
		
		if(i==sindex)			// pick up the selected point
			j=[raw count]-1;
		
		if(i==N-1 || ([[sub objectAtIndex:i] isEqualTo:[sub objectAtIndex:i+1]]==NO))
		{
			[[subcolors objectForKey:[sub objectAtIndex:i]] set];
			
			if(tindex==1)
				[self fitVonBertalanffy:raw];
			else
				if(tindex==2)
					[self fitSpline:raw];
			
			if(rindex==0)
				[self drawTimecourse:raw selectedIndex:j];
			else
				if(rindex==1)
					[self drawPhaseDiagram:raw selectedIndex:j];
			
			raw=[NSMutableArray new];
			j=-1;
		}
	}
}
-(void)drawTimecourse:(NSMutableArray*)raw selectedIndex:(int)j
{
	NSRect			r=[self frame];
	int				i;
	NSPoint			p;
	NSBezierPath	*bp=[NSBezierPath bezierPath];
	
	for(i=0;i<[raw count];i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		p=(NSPoint){(p.x-tmin)*r.size.width/(tmax-tmin),(p.y-vmin)*r.size.height/(vmax-vmin)};
		if(i==j)
			NSRectFill((NSRect){{p.x-3,p.y-3},{6,6}});
		else
			NSFrameRect((NSRect){{p.x-2,p.y-2},{4,4}});
		if(i==0)
			[bp moveToPoint:p];
		else
			[bp lineToPoint:p];
	}
	[bp stroke];
}
-(void)drawPhaseDiagram:(NSMutableArray*)raw selectedIndex:(int)j
{
	NSRect			r=[self frame];
	int				i;
	float			x0,x1,y0,y1,x,dv;
	NSPoint			p;
	NSBezierPath	*bp=[NSBezierPath bezierPath];
	
	for(i=0;i<[raw count];i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		if(i==0)
		{
			x0=p.x;
			y0=p.y;
		}
		else
		{
			x1=p.x;
			y1=p.y;
			dv=(y1-y0)/(x1-x0);
			x=(y0+y1)/2.0;
			
			p=(NSPoint){(x-vmin)*r.size.width/(vmax-vmin),(dv-dvmin)*r.size.height/(dvmax-dvmin)};
			if(i==j)
				NSRectFill((NSRect){{p.x-3,p.y-3},{6,6}});
			else
				NSFrameRect((NSRect){{p.x-2,p.y-2},{4,4}});
			if(i==1)
				[bp moveToPoint:p];
			else
				[bp lineToPoint:p];
			x0=x1;
			y0=y1;
		}
	}
	[bp stroke];
}
-(NSDictionary*)hitTimecourseAtPoint:(NSPoint)m
{
	NSRect			r=[self frame];
	int				i,N;
	float			x,y;
	NSArray			*sub=[data objectForKey:@"Subject"];
	NSArray			*age=[data objectForKey:@"Age"];
	NSArray			*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	NSPoint			p;
	
	N=[age count];
	
	// draw subject data
	for(i=0;i<N;i++)
	{
		x=[[age objectAtIndex:i] floatValue];
		y=[[var objectAtIndex:i] floatValue];
		p=(NSPoint){(x-tmin)*r.size.width/(tmax-tmin),(y-vmin)*r.size.height/(vmax-vmin)};
		if(NSPointInRect(m,(NSRect){{p.x-2,p.y-2},{4,4}}))
			return [NSDictionary dictionaryWithObjectsAndKeys:[sub objectAtIndex:i],@"sub",
					[age objectAtIndex:i],@"age",
					[NSNumber numberWithInt:i],@"i",nil];
	}
	return NULL;
}
-(NSDictionary*)hitPhaseDiagramAtPoint:(NSPoint)m
{
	NSRect			r=[self frame];
	int				N,i;
	NSArray			*sub=[data objectForKey:@"Subject"];
	NSArray			*age=[data objectForKey:@"Age"];
	NSArray			*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	NSString		*s=@"EMPTY";
	float			x0,x1,y0,y1,x,dv;
	NSPoint			p;
	
	N=[age count];
	
	// draw subject data
	for(i=0;i<N;i++)
	{
		if([s isEqualTo:[sub objectAtIndex:i]]==NO)
		{
			x0=[[age objectAtIndex:i] floatValue];
			y0=[[var objectAtIndex:i] floatValue];
		}
		else
		{
			x1=[[age objectAtIndex:i] floatValue];
			y1=[[var objectAtIndex:i] floatValue];
			dv=(y1-y0)/(x1-x0);
			x=(y0+y1)/2.0;
			
			p=(NSPoint){(x-vmin)*r.size.width/(vmax-vmin),(dv-dvmin)*r.size.height/(dvmax-dvmin)};
			if(NSPointInRect(m,(NSRect){{p.x-2,p.y-2},{4,4}}))
				return [NSDictionary dictionaryWithObjectsAndKeys:[sub objectAtIndex:i],@"sub",
						[age objectAtIndex:i],@"age",
						[NSNumber numberWithInt:i],@"i",nil];
			
			x0=x1;
			y0=y1;
		}
		s=[sub objectAtIndex:i];
	}
	
	return NULL;
}
#pragma mark -
void vonbertalanffy(float x, float a[], float *y, float dyda[], int na)
{
	float ex=exp(-x/a[2]);
	*y=a[1]*(1-ex);
	dyda[1]=1-ex;
	dyda[2]=-a[1]*x*ex/pow(a[2],2);
}
-(void)fitVonBertalanffy:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
	float	a[3]={0,1000000,500};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		x[i+1]=p.x+270;
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,vonbertalanffy,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,vonbertalanffy,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		p.x=x[i+1]-270;
		p.y=a[1]*(1-exp(-x[i+1]/a[2]));
		[raw addObject:NSStringFromPoint(p)];
	}
}
/*-(void)fitSpline:(NSMutableArray*)raw
 {
 int		ndata=[raw count];
 float	*x;
 float	*y,*y2;
 float	yy;
 int		i;
 NSPoint	p;
 
 x=(float*)calloc(ndata+1,sizeof(float));
 y=(float*)calloc(ndata+1,sizeof(float));
 y2=(float*)calloc(ndata+1,sizeof(float));
 for(i=0;i<ndata;i++)
 {
 p=NSPointFromString([raw objectAtIndex:i]);
 x[i+1]=p.x;
 y[i+1]=p.y;
 }
 spline(x,y,ndata,10e+30,10e+30,y2);
 
 [raw removeAllObjects];
 for(i=0;i<ndata;i++)
 {
 p.x=x[i+1];
 splint(x,y,y2,ndata,p.x,&yy);
 p.y=yy;
 [raw addObject:NSStringFromPoint(p)];
 }
 }
 */
-(void)fitSpline:(NSMutableArray*)raw
{
	const size_t n = [raw count];	// number of data points
	const size_t ncoeffs = 5;		// number of fit coefficients
	const size_t nbreak = ncoeffs-2;	// nbreak = ncoeffs + 2 - k, for k = 4 (cubic spline)
	size_t i, j;
	gsl_bspline_workspace *bw;
	gsl_vector *B;
	gsl_rng *r;
	gsl_vector *c, *w;
	gsl_vector *x, *y;
	gsl_matrix *X, *cov;
	gsl_multifit_linear_workspace *mw;
	double chisq, Rsq, dof, tss;
	
	NSPoint p,pstart,pend;
	pstart=NSPointFromString([raw objectAtIndex:0]);
	pend=NSPointFromString([raw lastObject]);
	
	gsl_rng_env_setup();
	r = gsl_rng_alloc(gsl_rng_default);
	
	/* allocate a cubic bspline workspace (k = 4) */
	bw = gsl_bspline_alloc(4, nbreak);
	B = gsl_vector_alloc(ncoeffs);
	
	x = gsl_vector_alloc(n);
	y = gsl_vector_alloc(n);
	X = gsl_matrix_alloc(n, ncoeffs);
	c = gsl_vector_alloc(ncoeffs);
	w = gsl_vector_alloc(n);
	cov = gsl_matrix_alloc(ncoeffs, ncoeffs);
	mw = gsl_multifit_linear_alloc(n, ncoeffs);
	
	printf("#m=0,S=0\n");
	/* this is the data to be fitted */
	for (i = 0; i < n; ++i)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		double xi = p.x;
		double yi = p.y;
		gsl_vector_set(x, i, xi);
		gsl_vector_set(y, i, yi);
		gsl_vector_set(w, i, 1.0);
		
		printf("%f %f\n", xi, yi);
	}
	
	/* use uniform breakpoints on [0, 15] */
	gsl_bspline_knots_uniform(pstart.x,pend.x, bw);
	
	/* construct the fit matrix X */
	for (i = 0; i < n; ++i)
	{
		double xi = gsl_vector_get(x, i);
		
		/* compute B_j(xi) for all j */
		gsl_bspline_eval(xi, B, bw);
		
		/* fill in row i of X */
		for (j = 0; j < ncoeffs; ++j)
		{
			double Bj = gsl_vector_get(B, j);
			gsl_matrix_set(X, i, j, Bj);
		}
	}
	
	/* do the fit */
	gsl_multifit_wlinear(X, w, y, c, cov, &chisq, mw);
	
	dof = n - ncoeffs;
	tss = gsl_stats_wtss(w->data, 1, y->data, 1, y->size);
	Rsq = 1.0 - chisq / tss;
	
	fprintf(stderr, "chisq/dof = %e, Rsq = %f\n", 
			chisq / dof, Rsq);
	
	/* output the smoothed curve */
	[raw removeAllObjects];
	{
		double xi,yi, yerr;
		
		printf("#m=1,S=0\n");
		for(i=0;i<n;i++)
		{
			xi=gsl_vector_get(x,i);
			gsl_bspline_eval(xi, B, bw);
			gsl_multifit_linear_est(B, c, cov, &yi, &yerr);
			printf("%f %f\n", xi, yi);
			[raw addObject:NSStringFromPoint((NSPoint){gsl_vector_get(x,i),yi})];
		}
	}
	
	gsl_rng_free(r);
	gsl_bspline_free(bw);
	gsl_vector_free(B);
	gsl_vector_free(x);
	gsl_vector_free(y);
	gsl_matrix_free(X);
	gsl_vector_free(c);
	gsl_vector_free(w);
	gsl_matrix_free(cov);
	gsl_multifit_linear_free(mw);
}
#pragma mark -
-(void)setApp:(id)theApp
{
	app=theApp;
}
-(void)displayMessage:(NSString *)theMsg
{
	NSDictionary	*dic;
	dic=[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"\n%@\n",theMsg] forKey:@"string"];
	[[NSNotificationCenter defaultCenter]	postNotificationName:@"MyPrintString"
														object:self
													  userInfo:dic];
}
-(void)commands
{
	printf("> list of available commands\n");
	NSArray			*cmds=[app cmds],*args;
	NSMutableString	*str=[NSMutableString new];
	int				i,j;
	
	[str appendString:@"\n"];
	for(i=0;i<[cmds count];i++)
	{
		[str appendString:[[cmds objectAtIndex:i] objectForKey:@"cmd"]];
		[str appendString:@"("];
		args=[[cmds objectAtIndex:i] objectForKey:@"args"];
		for(j=0;j<[args count];j++)
		{
			[str appendString:[args objectAtIndex:j]];
			if(j<[args count]-1)
				[str appendString:@","];
		}
		[str appendString:@")\n"];
	}
	
	[self displayMessage:str];
	[str release];
}
-(void)help:(char*)cmd
{
	printf("> help on command\n");
	NSArray			*cmds=[app cmds];
	NSMutableString	*str=[NSMutableString new];
	NSString		*hlp;
	int				i;
	
	for(i=0;i<[cmds count];i++)
		if([[[cmds objectAtIndex:i] objectForKey:@"cmd"] caseInsensitiveCompare:[NSString stringWithUTF8String:cmd]]==NSOrderedSame)
		{
			hlp=[[cmds objectAtIndex:i] objectForKey:@"help"];
			if([hlp length])
				[str appendString:hlp];
			else
				[str appendString:@"No help available"];
			break;
		}
	if(i==[cmds count])
		[str appendString:@"Command unknown"];
	
	//[str appendString:@"\n"];
	[self displayMessage:str];
	[str release];
}
-(void)laplace:(char*)var:(char*)sub:(int)smax
{
	int		i,j,n;
	NSArray	*subj=[data objectForKey:@"Subject"];
	NSArray	*age=[data objectForKey:@"Age"];
	NSArray	*arr2=[data objectForKey:@"Variables"];
	NSArray	*arr3;
	float	*x,*y,*s;
	
	// Find variable
	for(j=0;j<[arr2 count];j++)
		if([[[arr2 objectAtIndex:j] objectForKey:@"name"] isEqualTo:[NSString stringWithUTF8String:var]])
			break;
	if(j==[arr2 count])
	{
		[self displayMessage:@"ERROR: Variable not found"];
		return;
	}
	arr3=[[arr2 objectAtIndex:j] objectForKey:@"vals"];
	
	// Find variable values for subject
	for(i=0;i<[subj count];i++)
		if([[subj objectAtIndex:i] isEqualTo:[NSString stringWithUTF8String:sub]])
			printf("%g\t%s\n",[[age objectAtIndex:i] floatValue],[[arr3 objectAtIndex:i] UTF8String]);
	
	// compute numeric Laplace transform
	/*
	 s=(float*)calloc(smax,sizeof(float));
	 for(j=0;j<smax;j++)
	 {
	 for(i=xmin;i<=xmax;i++)
	 s[j]+=(y0*l0+y1*l1)/2*(x1-x0);
	 }
	 */
}
-(void)subjects
{
	int	i;
	NSString		*str0,*str1;
	NSMutableString	*str=[NSMutableString new];
	NSArray			*arr=[data objectForKey:@"Subject"];
	
	for(i=0;i<[arr count];i++)
	{
		str1=[arr objectAtIndex:i];
		if(i>0 && [str1 isEqualTo:str0])
			continue;
		[str appendString:str1];
		str0=str1;
		if(i<[arr count]-1)
			[str appendString:@"\n"];
	}
	[self displayMessage:str];
	[str release];
}
-(void)variables
{
	int	i;
	NSMutableString	*str=[NSMutableString new];
	NSArray			*arr=[data objectForKey:@"Variables"];
	
	for(i=0;i<[arr count];i++)
	{
		[str appendString:[[arr objectAtIndex:i] objectForKey:@"name"]];
		if(i<[arr count]-1)
			[str appendString:@"\n"];
	}
	[self displayMessage:str];
	[str release];
}
@end
