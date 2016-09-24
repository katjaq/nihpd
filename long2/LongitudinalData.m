//
//  LongitudinalData.m
//  long
//
//  Created by roberto on 18/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "LongitudinalData.h"


@implementation LongitudinalData
-(id)init
{
	self=[super init];
	if(self)
	{
		data=NULL;
	}
	return self;
}
#pragma mark -
-(void)readData:(NSURL*)fileURL
{
	if(fileURL==NULL)
		return;
	
	if(data!=NULL)
		[data release];
	data=[NSMutableDictionary new];
	
	FILE				*f;
	char				*ptr,str[1024];
	int					m,n,ind[2]={-1,-1};
	NSMutableArray		*arr,*var,*sub,*age;
	
	f=fopen([[fileURL path] UTF8String],"r");
	
	// find column names
	fgets(str,1024,f);
	m=0;
	var=[NSMutableArray new];
	ptr=strtok(str," ,\t\n");
	while(ptr!=NULL)
	{
		printf("%s\n",ptr);
		
		if(strcmp(ptr,"Subject")==0)
			ind[0]=m;
		else
			if(strcmp(ptr,"Age")==0)
				ind[1]=m;
			else
			{
				[var addObject:[NSDictionary dictionaryWithObjectsAndKeys:
								[NSString stringWithFormat:@"%s",ptr],@"name",
								[NSMutableArray new],@"vals",nil]];
			}
		
		ptr=strtok(NULL," ,\t\n");
		m++;
	}
	
	// find Subject and Age columns
	if(ind[0]<0)
	{
		printf("ERROR: Subject column not found\n");
		return;
	}
	if(ind[1]<0)
	{
		printf("ERROR: Age column not found\n");
		return;
	}
	sub=[NSMutableArray new];
	age=[NSMutableArray new];
	
	// add variable arrays to data
	[data setObject:sub forKey:@"Subject"];
	[data setObject:age forKey:@"Age"];
	[data setObject:var forKey:@"Variables"];
	
	// get values
	do
	{
		if(fgets(str,1024,f)==NULL)
			continue;
		
		m=n=0;
		ptr=strtok(str," ,\t\n");
		while(ptr!=NULL)
		{			
			if(m==ind[0])
				[sub addObject:[NSString stringWithUTF8String:ptr]];
			else
			if(m==ind[1])
				[age addObject:[NSNumber numberWithFloat:atof(ptr)]];
			else
			{
				arr=[[var objectAtIndex:n] objectForKey:@"vals"];
				[arr addObject:[NSString stringWithUTF8String:ptr]];
				//[arr addObject:[NSString stringWithFormat:@"%i",atof(ptr)+0.5]];
				n++;
			}
			
			ptr=strtok(NULL," ,\t\n");
			m++;
		}
	}
	while(!feof(f));

	[self findLimitsForAge];
	[self initColorsForSubjects];
}
-(void)initColorsForSubjects
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
-(NSMutableDictionary*)subcolors
{
	return subcolors;
}
-(NSDictionary*)findLimitsForAge
{
	int				N,i;
	NSArray			*age=[data objectForKey:@"Age"];
	float			x;
	float			tmin,tmax;
	
	tmin=tmax=[[age objectAtIndex:0] floatValue];
	N=[age count];
	for(i=0;i<N;i++)
	{
		x=[[age objectAtIndex:i] floatValue];
		
		if(x<tmin)	tmin=x;
		if(x>tmax)	tmax=x;
	}
	return [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithFloat:tmin],@"min",[NSNumber numberWithFloat:tmax],@"max",nil];
}
-(NSDictionary*)findLimitsForVariableAtIndex:(int)vindex
{
	int		N,i;
	NSArray	*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	float	y;
	float	vmin,vmax;
	
	vmin=vmax=[[var objectAtIndex:0] floatValue];
	N=[var count];
	for(i=0;i<N;i++)
	{
		y=[[var objectAtIndex:i] floatValue];
		
		if(y<vmin)	vmin=y;
		if(y>vmax)	vmax=y;
	}
	return [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithFloat:vmin],@"min",[NSNumber numberWithFloat:vmax],@"max",nil];
}
-(NSDictionary*)findLimitsForVariableDerivateAtIndex:(int)vindex
{
	int			N,i;
	NSArray		*sub=[data objectForKey:@"Subject"];
	NSArray		*age=[data objectForKey:@"Age"];
	NSArray		*var=[[[data objectForKey:@"Variables"] objectAtIndex:vindex] objectForKey:@"vals"];
	NSString	*s=@"EMPTY";
	float		x0,x1,y0,y1,dv;
	float		dvmin,dvmax;
	
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
	return [NSDictionary dictionaryWithObjectsAndKeys:[NSNumber numberWithFloat:dvmin],@"min",[NSNumber numberWithFloat:dvmax],@"max",nil];
}
#pragma mark -
-(NSArray*)subjectArray
{
	int	i;
	NSString		*str0,*str1;
	NSMutableString	*str=[NSMutableString new];
	NSArray			*arr=[data objectForKey:@"Subject"];
	NSMutableArray	*sub=[NSMutableArray arrayWithCapacity:10];
	
	for(i=0;i<[arr count];i++)
	{
		str1=[arr objectAtIndex:i];
		if(i>0 && [str1 isEqualTo:str0])
			continue;
		[sub addObject:str1];
		str0=str1;
	}
	[str release];
	
	return sub;
}

-(NSMutableArray*)dataForSubject:(NSString*)name variableIndex:(int)vindex
{
	int	i;
	NSArray			*sub=[self sub];
	NSArray			*age=[self age];
	NSArray			*var=[[[self var] objectAtIndex:vindex] objectForKey:@"vals"];
	float			x,y;
	NSMutableArray	*raw=[NSMutableArray arrayWithCapacity:10];
	
	for(i=0;i<[sub count];i++)
	{
		if([[sub objectAtIndex:i] isEqual:name])
		{
			x=[[age objectAtIndex:i] floatValue];
			y=[[var objectAtIndex:i] floatValue];
			[raw addObject:NSStringFromPoint((NSPoint){x,y})];
		}
	}
	return raw;
}
-(NSDictionary*)variableAtIndex:(int)vindex
{
	return [[self var] objectAtIndex:vindex];
}
-(int)indexForVariableName:(NSString*)vname
{
	int		i;
	NSArray	*var=[self var];
	
	for(i=0;i<[var count];i++)
	{
		if([[[var objectAtIndex:i] objectForKey:@"name"] isEqualTo:vname])
			break;
	}
	if(i==[var count])
	{
		printf("ERROR: Unknown variable in valuesForVariableName\n");
		return 0;
	}
	return i;	
}
-(NSMutableArray*)dataForVariableName:(char*)vname
{
	int		i;
	NSArray	*var=[self var];
	
	for(i=0;i<[var count];i++)
	{
		if([[[var objectAtIndex:i] objectForKey:@"name"] isEqualTo:[NSString stringWithUTF8String:vname]])
			break;
	}
	if(i==[var count])
	{
		printf("ERROR: Unknown variable in valuesForVariableName\n");
		return nil;
	}
	return [[var objectAtIndex:i] objectForKey:@"vals"];
}

#pragma mark -
void asymptotic(float x, float a[], float *y, float dyda[], int na)
{
	//a[1]=A
	//a[2]=T
	//a[3]=r
	float	K=(a[3]-1)*x/a[2]+1;
	float	K2=1/(1-a[3]);
	*y=a[1]*(1-pow(K,K2));
	dyda[1]=1-pow(K,K2);
	dyda[2]=-a[1]*x*pow(K,a[3]*K2)/(a[2]*a[2]);
	dyda[3]=-a[1]*pow(K,a[3]*K2)/pow(a[3]-1,2)*(K*(log(K)-1)+1);
}
-(NSDictionary*)fitAsymptotic:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=3;
	float	*x;
	float	*y;
	float	*sig;
	float	a[4]={0,45,6,1.5};
	int		ia[4]={0,1,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		//p.x=x[i+1]-280; // 280 days of gestation
		p.x=x[i+1]-9; // 9 months of gestation
		p.y=a[1]*(1-pow((a[3]-1)*x[i+1]/a[2]+1,1/(1-a[3])));
		[raw addObject:NSStringFromPoint(p)];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"A\tTau\talpha\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g\t%g\t%g",a[1],a[2],a[3],chisq],@"data",nil];
	return dic;
}
-(NSDictionary*)predictedAsymptotic:(NSMutableArray*)raw subject:(NSString*)sub
{
	int		ndata=[raw count];
	int		ma=3;
	float	*x;
	float	*y;
	float	*sig;
	float	a[4]={0,45,6,1.5};
	int		ia[4]={0,1,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p,p1;
	NSDictionary	*dic;
	NSMutableString	*str=[NSMutableString new];
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		p1.x=x[i+1]-9; // 9 months of gestation
		p1.y=a[1]*(1-pow((a[3]-1)*x[i+1]/a[2]+1,1/(1-a[3])));
		[raw addObject:NSStringFromPoint(p1)];
		
		p=NSPointFromString([raw objectAtIndex:i]);
		[str appendString:[NSString stringWithFormat:@"%@\t%g\t%g\n",sub,p1.x,p1.y]];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:
		 @"Subject\tAge\tPredicted\n",@"header",
		 str,@"data",
		 nil];
	return dic;
}
void asymptotic2(float x, float a[], float *y, float dyda[], int na)
{
	//a[1]=A
	//a[2]=T
	float	alpha=(3.09-0.28*a[2]>1.00001)?(3.09-0.28*a[2]):1.00001;
	float	K=(alpha-1)*x/a[2]+1;
	float	K2=1/(1-alpha);
	
	//printf("A:%g\tT:%g\ta:%g\n",a[1],a[2],alpha);
	*y=a[1]*(1-pow(K,K2));
	dyda[1]=1-pow(K,K2);
	dyda[2]=-a[1]*x*pow(K,alpha*K2)/(a[2]*a[2]);
}
-(NSDictionary*)fitAsymptotic2:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
	float	a[3]={0,45,6};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic2,&alamda);
	for(i=0;i<50;i++)
	{
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic2,&alamda);
		if(alamda==0)
			break;
	}
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		//p.x=x[i+1]-280; // 280 days of gestation
		p.x=x[i+1]-9; // 9 months of gestation
		p.y=a[1]*(1-pow((a[3]-1)*x[i+1]/a[2]+1,1/(1-a[3])
						));
		[raw addObject:NSStringFromPoint(p)];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"A\tTau\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g\t%g",a[1],a[2],chisq],@"data",nil];
	return dic;
}
-(NSDictionary*)predictedAsymptotic2:(NSMutableArray*)raw subject:(NSString*)sub
{
	// alpha fixed to 3.09-0.28*T, based on 36 subjects
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
	float	a[4]={0,45,6};
	int		ia[4]={0,1,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p,p1;
	NSDictionary	*dic;
	NSMutableString	*str=[NSMutableString new];
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic2,&alamda);
	for(i=0;i<50;i++)
	{
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,asymptotic2,&alamda);
		if(alamda==0)
			break;
	}
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	float al;
	for(i=0;i<ndata;i++)
	{
		p1.x=x[i+1]-9; // 9 months of gestation
		al=3.09-0.28*a[2];
		p1.y=a[1]*(1-pow((al-1)*x[i+1]/a[2]+1,1/(1-al)));
		[raw addObject:NSStringFromPoint(p1)];
		
		p=NSPointFromString([raw objectAtIndex:i]);
		[str appendString:[NSString stringWithFormat:@"%@\t%g\t%g\n",sub,p1.x,p1.y]];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:
		 @"Subject\tAge\tPredicted\n",@"header",
		 str,@"data",
		 nil];
	return dic;
}
void vonbertalanffy(float x, float a[], float *y, float dyda[], int na)
{
	float ex=exp(-x/a[2]);
	*y=a[1]*(1-ex);
	dyda[1]=1-ex;
	dyda[2]=-a[1]*x*ex/pow(a[2],2);
}
-(NSDictionary*)fitVonBertalanffy:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
//	float	a[3]={0,1000000,500};
	float	a[3]={0,45,6};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
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
		//p.x=x[i+1]-280; // 280 days of gestation
		p.x=x[i+1]-9; // 9 months of gestation
		p.y=a[1]*(1-exp(-x[i+1]/a[2]));
		[raw addObject:NSStringFromPoint(p)];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"A\tTau\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g\t%g",a[1],a[2],chisq],@"data",nil];
	return dic;
}
-(NSDictionary*)predictedVonBertalanffy:(NSMutableArray*)raw subject:(NSString*)sub
{
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
	float	a[3]={0,45,6};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p,p1;
	NSDictionary	*dic;
	NSMutableString	*str=[NSMutableString new];
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
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
		p1.x=x[i+1]-9; // 9 months of gestation
		p1.y=a[1]*(1-exp(-x[i+1]/a[2]));
		[raw addObject:NSStringFromPoint(p1)];

		p=NSPointFromString([raw objectAtIndex:i]);
		[str appendString:[NSString stringWithFormat:@"%@\t%g\t%g\n",sub,p1.x,p1.y]];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:
		 @"Subject\tAge\tPredicted\n",@"header",
		 str,@"data",
		 nil];
	return dic;
}
void henri(float x, float a[], float *y, float dyda[], int na)
{
	*y=a[1]*x/(a[2]+x);
	dyda[1]=x/(a[2]+x);
	dyda[2]=-a[1]*x/pow(a[2]+x,2);
}
-(NSDictionary*)fitHenri:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
	//	float	a[3]={0,1000000,500};
	float	a[3]={0,60,5};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,henri,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,henri,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		//p.x=x[i+1]-280; // 280 days of gestation
		p.x=x[i+1]-9; // 9 months of gestation
		p.y=a[1]*x[i+1]/(a[2]+x[i+1]);
		[raw addObject:NSStringFromPoint(p)];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"A\tTau\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g\t%g",a[1],a[2],chisq],@"data",nil];
	return dic;
}
-(NSDictionary*)predictedHenri:(NSMutableArray*)raw subject:(NSString *)sub
{
	int		ndata=[raw count];
	int		ma=2;
	float	*x;
	float	*y;
	float	*sig;
	//	float	a[3]={0,1000000,500};
	float	a[3]={0,60,5};
	int		ia[3]={0,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p,p1;
	NSDictionary	*dic;
	NSMutableString	*str=[NSMutableString new];
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,henri,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,henri,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		p1.x=x[i+1]-9; // 9 months of gestation
		p1.y=a[1]*x[i+1]/(a[2]+x[i+1]);
		[raw addObject:NSStringFromPoint(p1)];

		p=NSPointFromString([raw objectAtIndex:i]);
		[str appendString:[NSString stringWithFormat:@"%@\t%g\t%g\n",sub,p1.x,p1.y]];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:
			@"Subject\tAge\tPredicted\n",@"header",
			str,@"data",
			nil];
	return dic;
}
void gompertz(float x, float a[], float *y, float dyda[], int na)
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
void gompertzA(float x, float a[], float *y, float dyda[], int na)
{
	// a[1]: asymptote
	float	b=4;						// time displacement: 4
	float	c=122 + 7.1*a[1]/100000.0;	// growth rate: 122 + 7.1*A/100000.0;
	float	ex=exp(-b*exp(-x/c));
	*y=a[1]*ex;
	dyda[1]=ex;
	dyda[2]=-a[1]*ex*exp(-x/c);
	dyda[3]=-a[1]*b*x/pow(c,2)*ex*exp(-x/c);
}
-(NSDictionary*)fitGompertz:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=3;
	float	*x;
	float	*y;
	float	*sig;
	//float	a[4]={0,1000000,10,100};
	float	a[4]={0,60,1,10};
	int		ia[4]={0,1,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280;	// 280 days of gestation
		x[i+1]=p.x+9;	// 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,gompertz,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,gompertz,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		printf("%g\t%g\n",p.x,p.y);
	}
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		//p.x=x[i+1]-280; // 280 days of gestation
		p.x=x[i+1]-9; // 9 months of gestation
		p.y=a[1]*exp(-a[2]*exp(-x[i+1]/a[3]));
		
		printf("%g\n",p.y);
		[raw addObject:NSStringFromPoint(p)];
	}

	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"A\tB\tC\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g\t%g\t%g",a[1],a[2],a[3],chisq],@"data",nil];
	return dic;
}
-(NSDictionary*)fitGompertzA:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=1;
	float	*x;
	float	*y;
	float	*sig;
	//float	a[2]={0,1000000};
	float	a[2]={0,60};
	int		ia[2]={0,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		//x[i+1]=p.x+280; // 280 days of gestation
		x[i+1]=p.x+9; // 9 months of gestation
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,gompertzA,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,gompertzA,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	float	b=4;
	float	c=122+7.1*a[1]/100000.0;
	for(i=0;i<ndata;i++)
	{
		p.x=x[i+1]-280;
		p.y=a[1]*exp(-b*exp(-x[i+1]/c));
		[raw addObject:NSStringFromPoint(p)];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"A\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g",a[1],chisq],@"data",nil];
	return dic;
}
void logistic(float x, float a[], float *y, float dyda[], int na)
{
	// a[1]: K, carrying capacity
	// a[2]: r, Malthusian parameter
	// a[3]: d, delay
	float	ex=exp(a[2]*(x-a[3]));
	*y=a[1]*ex/(a[1]+ex-1);
	dyda[1]=ex*(ex-1)/pow(a[1]+ex-1,2);
	dyda[2]=(a[1]-1)*a[1]*(x-a[3])*ex/pow(a[1]+ex-1,2);
	dyda[3]=(a[1]-1)*a[1]*a[2]*ex/pow(ex+a[1]-1,2);
}
-(NSDictionary*)fitLogistic:(NSMutableArray*)raw
{
	int		ndata=[raw count];
	int		ma=3;
	float	*x;
	float	*y;
	float	*sig;
	float	a[4]={0,45,5,0};
	int		ia[4]={0,1,1,1};
	float	**covar;
	float	**alpha;
	float	chisq;
	float	alamda;
	int		i;
	NSPoint	p;
	NSDictionary	*dic;
	
	x=(float*)calloc(ndata+1,sizeof(float));
	y=(float*)calloc(ndata+1,sizeof(float));
	sig=(float*)calloc(ndata+1,sizeof(float));
	for(i=0;i<ndata;i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		x[i+1]=p.x+9;
		y[i+1]=p.y;
		sig[i+1]=1;
	}
	covar=matrix(1,ma,1,ma);
	alpha=matrix(1,ma,1,ma);
	alamda=-1;
	mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,logistic,&alamda);
	for(i=0;i<50;i++)
		mrqmin(x,y,sig,ndata,a,ia,ma,covar,alpha,&chisq,logistic,&alamda);
	free_matrix(covar,1,ma,1,ma);
	free_matrix(alpha,1,ma,1,ma);
	
	[raw removeAllObjects];
	for(i=0;i<ndata;i++)
	{
		p.x=x[i+1]-9;
		p.y=a[1]*exp(a[2]*(x[i+1]-a[3]))/(a[1]-1+exp(a[2]*(x[i+1]-a[3])));
		[raw addObject:NSStringFromPoint(p)];
	}
	
	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"K\tr\tDelta\tChi2",@"header",[NSString stringWithFormat:@"%g\t%g\t%g\t%g",a[1],a[2],a[3],chisq],@"data",nil];
	return dic;
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
-(NSDictionary*)fitSpline:(NSMutableArray*)raw
{
	const size_t n = [raw count];	// number of data points
	const size_t ncoeffs = 4;		// number of fit coefficients
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
	NSDictionary *dic;
	
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

	dic=[NSDictionary dictionaryWithObjectsAndKeys:@"Chi2",@"header",[NSString stringWithFormat:@"%g",chisq],@"data",nil];
	return dic;
}
-(void)setApp:(id)theApp
{
	app=theApp;
}
-(NSMutableArray*)sub
{
	return [data objectForKey:@"Subject"];
}
-(NSMutableArray*)age
{
	return [data objectForKey:@"Age"];
}
-(NSMutableArray*)var
{
	return [data objectForKey:@"Variables"];
}
-(int)sindex
{
	return sindex;
}
-(void)setSindex:(int)theSindex
{
	sindex=theSindex;
}
-(CGContextRef)createPDFContextWithRect:(CGRect)rect
{
	CGContextRef	pdfContext;
	CFStringRef		path;
	CFURLRef		url;
	char			*filename="/tmp/path.pdf";	

	path = CFStringCreateWithCString(NULL,filename,kCFStringEncodingUTF8);
	url = CFURLCreateWithFileSystemPath(NULL,path,kCFURLPOSIXPathStyle, 0);
	CFRelease (path);
	pdfContext = CGPDFContextCreateWithURL(url, &rect, NULL);
	CFRelease(url);
	CGContextBeginPage(pdfContext, &rect);

	// draw frame
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextStrokeRect(pdfContext,rect);
	
	//draw subject colors
	NSArray *arr=[self subjectArray];
	int	i;
	CGFloat	red,green,blue,alpha;
	for(i=0;i<[arr count];i++)
	{
		[[subcolors objectForKey:[arr objectAtIndex:i]] getRed:&red green:&green blue:&blue alpha:&alpha];
		CGContextSetRGBFillColor(pdfContext,red,green,blue,1);
		CGContextFillRect(pdfContext,(CGRect){i*8,0,7,7});
	}
	
	return pdfContext;
}
-(void)attachContext:(CGContextRef)pdfContext
{
	CGContextEndPage (pdfContext);
	CGContextRelease (pdfContext);
	
	CFStringRef		path;
	CFURLRef		url;
	char *filename="/tmp/path.pdf";	
	path = CFStringCreateWithCString(NULL,filename,kCFStringEncodingUTF8);
	url = CFURLCreateWithFileSystemPath(NULL,path,kCFURLPOSIXPathStyle, 0);	
	NSFileWrapper		*wrapper =[[NSFileWrapper alloc] initWithURL:(NSURL*)url options:0 error:nil];
	NSTextAttachment	*atta=[[NSTextAttachment alloc] initWithFileWrapper:wrapper];
	NSAttributedString	*a=[NSAttributedString attributedStringWithAttachment:atta];
	[self displayMessage:a];
	CFRelease(url);
}
-(void)plotGompertz:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color
{
	float	i,j,t,v,c[4],dyda[4];
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	sscanf([[dic objectForKey:@"data"] UTF8String]," %f %f %f ",&c[1],&c[2],&c[3]);
	for(i=0;i<rect.size.width;i++)
	{
		t=i*(lim[1]-lim[0])/rect.size.width+lim[0];
		//t+=280; // 280 days of gestation
		t+=9; // 9 months of gestation
		gompertz(t,c,&v,dyda,0);
		j=(v-lim[2])*rect.size.height/(lim[3]-lim[2]);
		if(i==0)
			CGContextMoveToPoint(pdfContext,i,j);
		else
			CGContextAddLineToPoint(pdfContext,i,j);
	}
	CGContextStrokePath(pdfContext);
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextAddRect(pdfContext,rect);
}
-(void)plotGompertzPhase:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color
{
	float	i,j,t,v,dv,c[4];
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	sscanf([[dic objectForKey:@"data"] UTF8String]," %f %f %f ",&c[1],&c[2],&c[3]);
	for(i=0;i<rect.size.width;i++)
	{
		v=i*(lim[1]-lim[0])/rect.size.width+lim[0];
		t=-c[3]*log(log(c[1]/v)/c[2]);
		dv=c[1]*c[2]/c[3]*exp(-c[2]*exp(-t/c[3])-t/c[3]);
		j=(dv-lim[2])*rect.size.height/(lim[3]-lim[2]);
		if(i==0)
			CGContextMoveToPoint(pdfContext,i,j);
		else
			CGContextAddLineToPoint(pdfContext,i,j);
	}
	CGContextStrokePath(pdfContext);
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextAddRect(pdfContext,rect);
}
-(void)plotGompertzA:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color
{
	float	i,j,t,v,c[4],dyda[4];
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	sscanf([[dic objectForKey:@"data"] UTF8String]," %f ",&c[1]);
	for(i=0;i<rect.size.width;i++)
	{
		t=i*(lim[1]-lim[0])/rect.size.width+lim[0];
		t+=280;
		gompertzA(t,c,&v,dyda,0);
		j=(v-lim[2])*rect.size.height/(lim[3]-lim[2]);
		if(i==0)
			CGContextMoveToPoint(pdfContext,i,j);
		else
			CGContextAddLineToPoint(pdfContext,i,j);
	}
	CGContextStrokePath(pdfContext);
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextAddRect(pdfContext,rect);
}
-(void)plotGompertzAPhase:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color
{
	float	i,j,t,v,dv,c[4];
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	sscanf([[dic objectForKey:@"data"] UTF8String]," %f ",&c[1]);
	c[2]=4;							// b
	c[3]=122+7.1*c[1]/100000.0;		// c
	for(i=0;i<rect.size.width;i++)
	{
		v=i*(lim[1]-lim[0])/rect.size.width+lim[0];
		t=-c[3]*log(log(c[1]/v)/c[2]);
		dv=c[1]*c[2]/c[3]*exp(-c[2]*exp(-t/c[3])-t/c[3]);
		j=(dv-lim[2])*rect.size.height/(lim[3]-lim[2]);
		if(i==0)
			CGContextMoveToPoint(pdfContext,i,j);
		else
			CGContextAddLineToPoint(pdfContext,i,j);
	}
	CGContextStrokePath(pdfContext);
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextAddRect(pdfContext,rect);
}

-(void)plotVonBertalanffy:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color
{
	float	i,j,t,v,c[4],dyda[4];
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	sscanf([[dic objectForKey:@"data"] UTF8String]," %f %f ",&c[1],&c[2]);
	for(i=0;i<rect.size.width;i++)
	{
		t=i*(lim[1]-lim[0])/rect.size.width+lim[0];
		//t+=280;
		t+=9;
		vonbertalanffy(t,c,&v,dyda,0);
		j=(v-lim[2])*rect.size.height/(lim[3]-lim[2]);
		if(i==0)
			CGContextMoveToPoint(pdfContext,i,j);
		else
			CGContextAddLineToPoint(pdfContext,i,j);
	}
	CGContextStrokePath(pdfContext);
}
-(void)plotVonBertalanffyPhase:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color
{
	float	i,j,t,v,dv,c[4];
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	sscanf([[dic objectForKey:@"data"] UTF8String]," %f %f ",&c[1],&c[2]);
	for(i=0;i<rect.size.width;i++)
	{
		v=i*(lim[1]-lim[0])/rect.size.width+lim[0];
		t=c[2]*log(c[1]/(c[1]-v));
		dv=c[1]*exp(-t/c[2])/c[2];
		j=(dv-lim[2])*rect.size.height/(lim[3]-lim[2]);
		if(i==0)
			CGContextMoveToPoint(pdfContext,i,j);
		else
			CGContextAddLineToPoint(pdfContext,i,j);
	}
	CGContextStrokePath(pdfContext);
}
-(void)plotTimecourse:(NSMutableArray*)raw context:(CGContextRef)pdfContext rect:(CGRect)r limits:(float*)lim color:(NSColor*)color
{
	int		i;
	NSPoint	p;
	CGFloat	red,green,blue,alpha;

	[color getRed:&red green:&green blue:&blue alpha:&alpha];

	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextSetRGBFillColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	for(i=0;i<[raw count];i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		p=(NSPoint){(p.x-lim[0])*r.size.width/(lim[1]-lim[0]),(p.y-lim[2])*r.size.height/(lim[3]-lim[2])};
		if(i==0)
			CGContextMoveToPoint(pdfContext,p.x,p.y);
		else
			CGContextAddLineToPoint(pdfContext,p.x,p.y);
	}
	CGContextStrokePath(pdfContext);

	for(i=0;i<[raw count];i++)
	{
		p=NSPointFromString([raw objectAtIndex:i]);
		p=(NSPoint){(p.x-lim[0])*r.size.width/(lim[1]-lim[0]),(p.y-lim[2])*r.size.height/(lim[3]-lim[2])};
		CGContextAddEllipseInRect(pdfContext,(CGRect){{p.x-1,p.y-1},{2,2}});
	}
	CGContextFillPath(pdfContext);
}
-(void)plotPhase:(NSMutableArray*)raw context:(CGContextRef)pdfContext rect:(CGRect)r limits:(float*)lim color:(NSColor*)color
{
	int		i;
	float	x0,x1,y0,y1,x,dv;
	NSPoint	p;
	CGFloat	red,green,blue,alpha;
	
	[color getRed:&red green:&green blue:&blue alpha:&alpha];
	
	CGContextSetRGBStrokeColor(pdfContext,red,green,blue,alpha);
	CGContextSetRGBFillColor(pdfContext,red,green,blue,alpha);
	CGContextBeginPath(pdfContext);
	printf("printing %s\n",[[raw description] UTF8String]);
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
			
			p=(NSPoint){(x-lim[0])*r.size.width/(lim[1]-lim[0]),(dv-lim[2])*r.size.height/(lim[3]-lim[2])};
			if(i==1)
				CGContextMoveToPoint(pdfContext,p.x,p.y);
			else
				CGContextAddLineToPoint(pdfContext,p.x,p.y);
			x0=x1;
			y0=y1;
		}
	}
	CGContextStrokePath(pdfContext);
	
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
			
			p=(NSPoint){(x-lim[0])*r.size.width/(lim[1]-lim[0]),(dv-lim[2])*r.size.height/(lim[3]-lim[2])};
			CGContextAddEllipseInRect(pdfContext,(CGRect){{p.x-1,p.y-1},{2,2}});
			x0=x1;
			y0=y1;
		}
	}
	CGContextFillPath(pdfContext);
}
#pragma mark -
float volumeroot(float A, float *param)
{
	// param[1]=b
	// param[2]=alpha
	// param[3]=beta
	// param[4]=t
	// param[5]=v
	return A*exp(-param[1]*exp(-param[4]/(param[2]+param[3]*A)))-param[5];
}
// sub count
// raw data for sub i
float	*rawdata;
int		nsub,ntotal;
int		*subLUT;
int zbrac(float (*func)(float,float*), float *x1, float *x2, float *param)
// Given a function func and an initial guessed range x1 to x2, the routine expands the range
// geometrically until a root is bracketed by the returned values x1 and x2 (in which case zbrac
// returns 1) or until the range becomes unacceptably large (in which case zbrac returns 0).
{
	int j,FACTOR,NTRY;
	float f1,f2;
	
	FACTOR=1.6;
	NTRY=50;
	
	if (*x1 == *x2)
		printf("Bad initial range in zbrac\n");
	f1=(*func)(*x1,param);
	f2=(*func)(*x2,param);
	for (j=1;j<=NTRY;j++)
	{
		if (f1*f2 < 0.0)
			return 1;
		if (fabs(f1) < fabs(f2))
			f1=(*func)(*x1 += FACTOR*(*x1-*x2),param);
		else
			f2=(*func)(*x2 += FACTOR*(*x2-*x1),param);
	}
	return 0;
}
float fitall_sqrerr(float *param)
{
	int				i,j,i0;
	float			b,alpha,beta,param2[5];
	float			v,t,A;
	double			sumA,sumA2;
	double			n,std,err;
	float			x1,x2;
	
	b=param[1];
	alpha=param[2];
	beta=param[3];
	
	param2[1]=b;
	param2[2]=alpha;
	param2[3]=beta;
	
	err=0;
	i0=0;
	for(i=0;i<nsub;i++)
	{
		sumA=0;
		sumA2=0;
		for(j=i0+1;j<subLUT[i];j++)		// +1 to skip v=t=0
		{
			t=rawdata[j*2+0];
			v=rawdata[j*2+1];
			param2[4]=t;
			param2[5]=v;
			x1=1e+5;
			x2=2e+6;
			zbrac(volumeroot,&x1,&x2,param2);
			A=zriddr(volumeroot,x1,x2,1,param2);
			sumA+=A;
			sumA2+=A*A;
		}
		n=(subLUT[i]-i0-1);				// -1 because I skipped v=t=0
		std=sumA2-sumA*sumA/n;
		printf("mean=%g\tstd=%g\n",sumA/n,std);
		err+=std;
		i0=subLUT[i];
	}
	printf("--------- error=%g\n",err);
	
	return (float)err;
}
#pragma mark -
#pragma mark [ Commands ]
-(void)displayMessage:(id)theMsg
{
	NSDictionary	*dic;
	dic=[NSDictionary dictionaryWithObject:theMsg forKey:@"string"];
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
-(void)fit:(char*)vname:(char*)model
{
	int				i,vindex;
	NSArray			*var=[self var];
	NSArray			*sub=[self subjectArray];
	NSMutableArray	*raw;
	NSDictionary	*dic;
	NSMutableString	*str=[NSMutableString new];

	for(i=0;i<[var count];i++)
		if([[[var objectAtIndex:i] objectForKey:@"name"] isEqualTo:[NSString stringWithUTF8String:vname]])
			break;
	if(i==[var count])
	{
		[self displayMessage:@"ERROR: Unknown variable name"];
		return;
	}
	vindex=i;
	
	for(i=0;i<[sub count];i++)
	{
		raw=[self dataForSubject:[sub objectAtIndex:i] variableIndex:vindex];

		if(strcmp(model,"asymp")==0)
			dic=[self fitAsymptotic:raw];
		else
		if(strcmp(model,"asymp2")==0)
			dic=[self fitAsymptotic2:raw];
		else
		if(strcmp(model,"henri")==0)
			dic=[self fitHenri:raw];
		else
		if(strcmp(model,"vonbertalanffy")==0)
			dic=[self fitVonBertalanffy:raw];
		else
			if(strcmp(model,"gompertz")==0)
		dic=[self fitGompertz:raw];
		else
		if(strcmp(model,"gompertzA")==0)
			dic=[self fitGompertzA:raw];
		else
		if(strcmp(model,"logistic")==0)
			dic=[self fitLogistic:raw];
		else
		if(strcmp(model,"spline")==0)
			dic=[self fitSpline:raw];
		else
		{
			[self displayMessage:@"ERROR: Unknown model"];
			return;
		}
		
		if(i==0)
			[str appendString:[NSString stringWithFormat:@"Sub\t%@\n",[dic objectForKey:@"header"]]];
			 [str appendString:[NSString stringWithFormat:@"%@\t%@",[sub objectAtIndex:i],[dic objectForKey:@"data"]]];
		if(i<[sub count]-1)
			[str appendString:@"\n"];
	}
	
	[self displayMessage:str];
	[str release];
}
-(void)predicted:(char*)vname:(char*)model
{
	int				i,vindex;
	NSArray			*var=[self var];
	NSArray			*sub=[self subjectArray];
	NSMutableArray	*raw;
	NSDictionary	*dic;
	NSMutableString	*str=[NSMutableString new];
	
	for(i=0;i<[var count];i++)
		if([[[var objectAtIndex:i] objectForKey:@"name"] isEqualTo:[NSString stringWithUTF8String:vname]])
			break;
	if(i==[var count])
	{
		[self displayMessage:@"ERROR: Unknown variable name"];
		return;
	}
	vindex=i;
	
	for(i=0;i<[sub count];i++)
	{
		raw=[self dataForSubject:[sub objectAtIndex:i] variableIndex:vindex];
		
		if(strcmp(model,"asymp")==0)
			dic=[self predictedAsymptotic:raw subject:[sub objectAtIndex:i]];
		else
		if(strcmp(model,"asymp2")==0)
			dic=[self predictedAsymptotic2:raw subject:[sub objectAtIndex:i]];
		else
		if(strcmp(model,"henri")==0)
			dic=[self predictedHenri:raw subject:[sub objectAtIndex:i]];
		else
		if(strcmp(model,"vonbertalanffy")==0)
			dic=[self predictedVonBertalanffy:raw subject:[sub objectAtIndex:i]];
		else
		{
			[self displayMessage:@"ERROR: Unknown model (or not implemented yet...)"];
			return;
		}
		
		if(i==0)
			[str appendString:[dic objectForKey:@"header"]];
		[str appendString:[dic objectForKey:@"data"]];
	}
	
	[self displayMessage:str];
	[str release];
}
-(void)fitall:(char*)vname:(char*)model
{
	NSMutableString	*str=[NSMutableString new];

	// fit all subjects to V=A*exp(-b*exp(-t/(alpha+beta*A))), params: b, alpha, beta
	int		i,j;
	int		ndim;
	int		nfunk;
	float	ftol=0.00001;
	float	p[3*4+1],f[4+1];
	
	NSArray			*var=[self var];
	NSArray			*age=[self age];
	NSArray			*sub=[self subjectArray];
	int				vindex=[self indexForVariableName:@"Volume"];
	NSMutableArray	*raw;
	
	// put the data in neutral territory
	nsub=[sub count];
	ntotal=[[self sub] count];
	rawdata=(float*)calloc(2*ntotal,sizeof(float));
	subLUT=(int*)calloc(nsub,sizeof(int));
	for(i=0;i<ntotal;i++)
	{
		rawdata[i*2+0]=[[age objectAtIndex:i] floatValue]+280;
		rawdata[i*2+1]=[[[[var objectAtIndex:vindex] objectForKey:@"vals"] objectAtIndex:i] floatValue];
	}
	for(i=0;i<nsub;i++)
	{
		raw=[self dataForSubject:[sub objectAtIndex:i] variableIndex:vindex];
		if(i==0)
			subLUT[i]=[raw count];
		else
			subLUT[i]=subLUT[i-1]+[raw count];
	}

	// ndim is the number of dimensions
	ndim=3;
	
	// the simplex p has ndim+1 vertices in ndim space
	// p0: (b,alpha,beta)=(4,120,7/10000.0)
	p[1]=4;			// b
	p[2]=120;		// alpha
	p[3]=7/100000.0;	// beta
	for(i=2;i<5;i++)
		for(j=1;j<4;j++)
			p[3*(i-1)+j]=p[j]*(1+0.1*(j==(i-1)));
	
	// evaluation of the error function at each of the simplex vertices
	for(i=1;i<5;i++)
		f[i]=fitall_sqrerr(&p[3*(i-1)]);
	
	amoeba(	p,				// ndimensional coordinates of the starting simplex
		   f,				// value of funk at the vertices of the simplex
		   ndim,			// number of dimensions
		   ftol,			// tolerance
		   fitall_sqrerr,	// funk: function to minimise
		   &nfunk			// number of function evaluations
		   );

	[str appendString:[NSString stringWithFormat:@"b\talpha\tbeta\n"]];
	[str appendString:[NSString stringWithFormat:@"%g\t%g\t%g\n",p[1],p[2],p[3]]];
	[str appendString:@"\n"];
	
	[self displayMessage:str];
	[str release];
	
	free(rawdata);
	free(subLUT);

	// plot
	CGRect	rect={0,0,200,100};
	CGContextRef pdfContext=[self createPDFContextWithRect:rect];

	float	A,x,y,t,v,c[4],dyda[4];
		
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextBeginPath(pdfContext);
	for(A=500000;A<2000000;A+=100000)
	{
		c[1]=A;					// A
		c[2]=4;//p[1];			// b
		c[3]=122-7.1*A/100000.0;	//p[2]+p[3]*c[1];	// c=alpha+beta*A
		for(x=0;x<rect.size.width;x++)
		{
			t=x*2000/rect.size.width;
			gompertz(t,c,&v,dyda,0);
			y=v*rect.size.height/(float)2e+6;
			if(x==0)
				CGContextMoveToPoint(pdfContext,x,y);
			else
				CGContextAddLineToPoint(pdfContext,x,y);
		}
	}
	CGContextStrokePath(pdfContext);
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextAddRect(pdfContext,rect);
	
	
	[self attachContext:pdfContext];
}
-(void)fitplot:(char*)vname:(char*)model:(char*)limits:(char*)type
{
	int				i,vindex;
	NSArray			*var=[self var];
	NSArray			*sub=[self subjectArray];
	NSMutableArray	*raw;
	NSDictionary	*dic;
	float			lim[4];
	
	sscanf(limits," %f %f %f %f ",&lim[0],&lim[1],&lim[2],&lim[3]);
	
	for(i=0;i<[var count];i++)
		if([[[var objectAtIndex:i] objectForKey:@"name"] isEqualTo:[NSString stringWithUTF8String:vname]])
			break;
	if(i==[var count])
	{
		[self displayMessage:@"ERROR: Unknown variable name"];
		return;
	}
	vindex=i;
	
	CGRect	rect={0,0,200,100};
	CGContextRef pdfContext=[self createPDFContextWithRect:rect];
	
	for(i=0;i<[sub count];i++)
	{
		raw=[self dataForSubject:[sub objectAtIndex:i] variableIndex:vindex];
		
		if(strcmp(model,"vonbertalanffy")==0)
		{
			dic=[self fitVonBertalanffy:raw];
			if(strcmp(type,"time")==0)
				[self plotVonBertalanffy:dic context:pdfContext rect:rect limits:lim color:[subcolors objectForKey:[sub objectAtIndex:i]]];
			else
			if(strcmp(type,"phase")==0)
				[self plotVonBertalanffyPhase:dic context:pdfContext rect:rect limits:lim color:[subcolors objectForKey:[sub objectAtIndex:i]]];
			else
			{
				[self displayMessage:@"ERROR: Unknown diagram type"];
				return;
			}
		}
		else
		if(strcmp(model,"gompertz")==0)
		{
			dic=[self fitGompertz:raw];
			if(strcmp(type,"time")==0)
				[self plotGompertz:dic context:pdfContext rect:rect limits:lim color:[subcolors objectForKey:[sub objectAtIndex:i]]];
			else
			if(strcmp(type,"phase")==0)
				[self plotGompertzPhase:dic context:pdfContext rect:rect limits:lim color:[subcolors objectForKey:[sub objectAtIndex:i]]];
			else
			{
				[self displayMessage:@"ERROR: Unknown diagram type"];
				return;
			}
		}
		else
		if(strcmp(model,"gompertzA")==0)
		{
			dic=[self fitGompertzA:raw];
			if(strcmp(type,"time")==0)
				[self plotGompertzA:dic context:pdfContext rect:rect limits:lim color:[subcolors objectForKey:[sub objectAtIndex:i]]];
			else
			if(strcmp(type,"phase")==0)
				[self plotGompertzAPhase:dic context:pdfContext rect:rect limits:lim color:[subcolors objectForKey:[sub objectAtIndex:i]]];
			else
			{
				[self displayMessage:@"ERROR: Unknown diagram type"];
				return;
			}
		}
		else
		if(strcmp(model,"logistic")==0)
			dic=[self fitLogistic:raw];
		else
		if(strcmp(model,"spline")==0)
			dic=[self fitSpline:raw];
		else
		{
			[self displayMessage:@"ERROR: Unknown model"];
			return;
		}
	}
	[self attachContext:pdfContext];
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
-(void)plot:(char*)vname:(char*)limits:(char*)type
{
	printf("---------------------PLOT\n");
	int					i;
	int					vindex=[self indexForVariableName:[NSString stringWithUTF8String:vname]];
	NSArray				*sub=[self subjectArray];
	NSMutableArray		*raw;
	CGRect				rect={0,0,200,100};
	float				lim[4];
	NSMutableDictionary	*col=[self subcolors];
	CGContextRef		pdfContext;
	
	sscanf(limits," %f %f %f %f ",&lim[0],&lim[1],&lim[2],&lim[3]);
	
	pdfContext=[self createPDFContextWithRect:rect];
	for(i=0;i<[sub count];i++)
	{
		printf("--------------------------sub %i\n",i);
		raw=[self dataForSubject:[sub objectAtIndex:i] variableIndex:vindex];
		if(strcmp(type,"time")==0)
			[self plotTimecourse:raw context:pdfContext rect:rect limits:lim color:[col valueForKey:[sub objectAtIndex:i]]];
		else
		if(strcmp(type,"phase")==0)
			[self plotPhase:raw context:pdfContext rect:rect limits:lim color:[col valueForKey:[sub objectAtIndex:i]]];
		else
		{
			[self displayMessage:@"ERROR: Unknown diagram type"];
			return;
		}
	}
	[self attachContext:pdfContext];
}
-(void)subjects
{
	int				i;
	NSArray			*arr=[self subjectArray];
	NSMutableString	*str=[NSMutableString new];
	
	for(i=0;i<[arr count];i++)
	{
		[str appendString:[arr objectAtIndex:i]];
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
#pragma mark -
float volumeroot2(float A, float *param)
{
	// param[1]=b
	// param[2]=alpha
	// param[3]=beta
	// param[4]=t
	// param[5]=v
	return A*exp(-param[1]*exp(-param[4]/(param[2]+param[3]*A)))-param[5];
}
float fitall2_sqrerr(float *param)
{
	int				i,j,i0;
	float			b,alpha,beta,param2[5];
	float			v,t,A;
	double			sumA,sumA2;
	double			n,std,err;
	float			x1,x2;
	
	b=param[1];
	alpha=param[2];
	beta=param[3];
	
	param2[1]=b;
	param2[2]=alpha;
	param2[3]=beta;
	
	err=0;
	i0=0;
	for(i=0;i<nsub;i++)
	{
		sumA=0;
		sumA2=0;
		for(j=i0+1;j<subLUT[i];j++)		// +1 to skip v=t=0
		{
			t=rawdata[j*2+0];
			v=rawdata[j*2+1];
			param2[4]=t;
			param2[5]=v;
			x1=1e+5;
			x2=2e+6;
			zbrac(volumeroot2,&x1,&x2,param2);
			A=zriddr(volumeroot2,x1,x2,1,param2);
			sumA+=A;
			sumA2+=A*A;
		}
		n=(subLUT[i]-i0-1);				// -1 because I skipped v=t=0
		std=sumA2-sumA*sumA/n;
		printf("mean=%g\tstd=%g\n",sumA/n,std);
		err+=std;
		i0=subLUT[i];
	}
	printf("--------- error=%g\n",err);
	
	return (float)err;
}
-(void)fitall2:(char*)vname:(char*)model
{
	NSMutableString	*str=[NSMutableString new];
	
	// fit all subjects to V=A*exp(-b*exp(-t/(alpha+beta*A))), params: b, alpha, beta
	int		i,j;
	int		ndim;
	int		nfunk;
	float	ftol=0.00001;
	float	p[3*4+1],f[4+1];
	
	NSArray			*var=[self var];
	NSArray			*age=[self age];
	NSArray			*sub=[self subjectArray];
	int				vindex=[self indexForVariableName:@"Volume"];
	NSMutableArray	*raw;
	
	// put the data in neutral territory
	nsub=[sub count];
	ntotal=[[self sub] count];
	rawdata=(float*)calloc(2*ntotal,sizeof(float));
	subLUT=(int*)calloc(nsub,sizeof(int));
	for(i=0;i<ntotal;i++)
	{
		rawdata[i*2+0]=[[age objectAtIndex:i] floatValue]+280;
		rawdata[i*2+1]=[[[[var objectAtIndex:vindex] objectForKey:@"vals"] objectAtIndex:i] floatValue];
	}
	for(i=0;i<nsub;i++)
	{
		raw=[self dataForSubject:[sub objectAtIndex:i] variableIndex:vindex];
		if(i==0)
			subLUT[i]=[raw count];
		else
			subLUT[i]=subLUT[i-1]+[raw count];
	}
	
	// ndim is the number of dimensions
	ndim=3;
	
	// the simplex p has ndim+1 vertices in ndim space
	// p0: (b,alpha,beta)=(4,120,7/10000.0)
	p[1]=4;			// b
	p[2]=120;		// alpha
	p[3]=7/100000.0;	// beta
	for(i=2;i<5;i++)
		for(j=1;j<4;j++)
			p[3*(i-1)+j]=p[j]*(1+0.1*(j==(i-1)));
	
	// evaluation of the error function at each of the simplex vertices
	for(i=1;i<5;i++)
		f[i]=fitall2_sqrerr(&p[3*(i-1)]);
	
	amoeba(	p,				// ndimensional coordinates of the starting simplex
		   f,				// value of funk at the vertices of the simplex
		   ndim,			// number of dimensions
		   ftol,			// tolerance
		   fitall2_sqrerr,	// funk: function to minimise
		   &nfunk			// number of function evaluations
		   );
	
	[str appendString:[NSString stringWithFormat:@"b\talpha\tbeta\n"]];
	[str appendString:[NSString stringWithFormat:@"%g\t%g\t%g\n",p[1],p[2],p[3]]];
	[str appendString:@"\n"];
	
	[self displayMessage:str];
	[str release];
	
	free(rawdata);
	free(subLUT);
	
	// plot
	CGRect	rect={0,0,200,100};
	CGContextRef pdfContext=[self createPDFContextWithRect:rect];
	
	float	A,x,y,t,v,c[4],dyda[4];
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextBeginPath(pdfContext);
	for(A=500000;A<2000000;A+=100000)
	{
		c[1]=A;					// A
		c[2]=4;//p[1];			// b
		c[3]=122-7.1*A/100000.0;	//p[2]+p[3]*c[1];	// c=alpha+beta*A
		for(x=0;x<rect.size.width;x++)
		{
			t=x*2000/rect.size.width;
			gompertz(t,c,&v,dyda,0);
			y=v*rect.size.height/(float)2e+6;
			if(x==0)
				CGContextMoveToPoint(pdfContext,x,y);
			else
				CGContextAddLineToPoint(pdfContext,x,y);
		}
	}
	CGContextStrokePath(pdfContext);
	
	CGContextSetRGBStrokeColor(pdfContext,0,0,0,1);
	CGContextAddRect(pdfContext,rect);
	
	
	[self attachContext:pdfContext];
}

@end
