//
//  MyView.m
//  nihpd303growth
//
//  Created by roberto on 26/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import "MyView.h"


@implementation MyView

- (id)initWithFrame:(NSRect)frame
{
    self = [super initWithFrame:frame];
    if (self)
	{
        [self loadData];
    }
    return self;
}

- (void)drawRect:(NSRect)dirtyRect
{
	[self drawDensity];
    [self drawData];
}
#pragma mark -
float* vol(int i, int j)
{
	return &data[(i*3+j)*3+0];
}
float* age(int i, int j)
{
	return &data[(i*3+j)*3+1];
}
float* sex(int i, int j)
{
	return &data[(i*3+j)*3+2];
}
-(void)loadData
{
	NSString *path=[NSString stringWithFormat:@"%@/nihpd303.txt",[[[NSBundle mainBundle] resourceURL] path]];
	FILE	*f;
	int		i,j;
	char	str[256],tmp[256];
	
	data=(float*)calloc(101*3*3,sizeof(float));
	f=fopen([path UTF8String],"r");
	fgets(str,255,f);
	for(i=0;i<101;i++)
		for(j=0;j<3;j++)
		{
			fgets(str,255,f);
			sscanf(str,"%*s %*s %f %f %*s %s",vol(i,j),age(i,j),tmp);
			
			*vol(i,j)/=1000.0;
			*age(i,j)/=365.0;
			if(strcmp(tmp,"Male")==0)
				*sex(i,j)=0;
			else
				*sex(i,j)=1;
		}
	
	vmin=1200;
	vmax=2100;
	amin=4.5;
	amax=22.5;
}
-(void)drawData
{
	NSRect			r=[self bounds];
	int				i,j;
	NSPoint			p[3];
	NSBezierPath	*bp;
	
	NSFrameRect(r);
	for(i=0;i<101;i++)
	{
		for(j=0;j<3;j++)
		{
			p[j]=(NSPoint){*age(i,j),*vol(i,j)};
			p[j].x=(p[j].x-amin)*r.size.width/(amax-amin);
			p[j].y=(p[j].y-vmin)*r.size.height/(vmax-vmin);
		}
		bp=[NSBezierPath bezierPath];
		[bp moveToPoint:p[0]];
		[bp lineToPoint:p[1]];
		[bp lineToPoint:p[2]];
		
		if(*sex(i,0)==0)
			[[NSColor blueColor] set];
		else
			[[NSColor redColor] set];

		[bp stroke];
	}
	
}
-(void)drawDensity
{
	NSRect	r=[self bounds];
	NSRect	r1;
	int		i,j,l;
	float	x,y,d,max;
	int		w,h;
	float	*den;
	float	*den1;
	float	*nor;
	float	R,G,B;
	NSColor	*c;
	
	w=200;
	h=200;
	
	den=(float*)calloc(w*h,sizeof(float));
	den1=(float*)calloc(w*h,sizeof(float));
	nor=(float*)calloc(w*h,sizeof(float));
	
	NSFrameRect(r);
	
	for(i=0;i<101;i++)
	{
		for(j=0;j<3;j++)
		{
			x=*age(i,j);
			y=*vol(i,j);
			
			den[w*(int)((y-vmin)*h/(vmax-vmin))+(int)((x-amin)*w/(amax-amin))]++;
			
			if(j==0 || j==1)
			{
				x=(*age(i,j)+*age(i,j+1))/2.0;
				y=(*vol(i,j)+*vol(i,j+1))/2.0;
				den[w*(int)((y-vmin)*h/(vmax-vmin))+(int)((x-amin)*w/(amax-amin))]++;
			}
		}
	}
	
	for(l=0;l<35;l++)
	{
		for(i=0;i<w;i++)
			for(j=0;j<h;j++)
			{
				if(i>0&&j>0)
					den1[w*j+i]+=den[w*(j-1)+(i-1)];
				if(j>0)
					den1[w*j+i]+=den[w*(j-1)+i];
				if(i<(w-1)&&j>0)
					den1[w*j+i]+=den[w*(j-1)+(i+1)];
				if(i>0)
					den1[w*j+i]+=den[w*j+(i-1)];
				den1[w*j+i]+=den[w*j+i];
				if(i<(w-1))
					den[w*j+i]+=den[w*j+(i+1)];
				if(i>0&&j<(h-1))
					den1[w*j+i]+=den[w*(j+1)+(i-1)];
				if(j<(h-1))
					den1[w*j+i]+=den[w*(j+1)+i];
				if(i<(w-1)&&j<(h-1))
					den1[w*j+i]+=den[w*(j-1)+(i+1)];
			}
		for(i=0;i<w*h;i++)
		{
			den[i]=den1[i];
			den1[i]=0;
		}
	}
	
	if(1)
	{
		for(i=0;i<w;i++)
			for(j=0;j<h;j++)
				nor[i]+=den[w*j+i];
		max=0;
		for(i=0;i<w;i++)
			if(nor[i])
				for(j=0;j<h;j++)
				{
					den[w*j+i]=den[w*j+i]/nor[i];
					if(den[w*j+i]>max)
						max=den[w*j+i];
				}
		printf("max=%f\n",max);
		max=max*0.5;
	}
	
	for(i=0;i<w;i++)
		for(j=0;j<h;j++)
		{
			r1=(NSRect){i*r.size.width/(float)w,j*r.size.height/(float)h,r.size.width/(float)w+1,r.size.height/(float)h+1};
			d=den[w*j+i]/max;
			R=(3*(d-2/3.0)*(d>=2/3.0));
			G=(3*(d-1/3.0)*(d>=1/3.0 && d<=2/3.0));
			B=(3*d*(d<=1/3.0));
			if(R>1) R=1;
			if(G>1) G=1;
			if(B>1) B=1;
			c=[NSColor colorWithDeviceRed:R green:G blue:B alpha:1];
			[c set];
			NSRectFill(r1);
		}
	
	free(den);
	free(den1);
	free(nor);
}
@end
