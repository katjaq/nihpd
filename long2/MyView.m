//
//  MyView.m
//  long
//
//  Created by roberto on 26/11/2010.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "MyView.h"
	
@implementation MyView

- (id)initWithFrame:(NSRect)frame
{
    self = [super initWithFrame:frame];
    if (self)
	{
        LData=nil;
		rindex=0;
		vindex=1;
    }
    return self;
}

- (void)drawRect:(NSRect)dirtyRect
{
	if(LData==NULL)
		return;
	
	[self drawData];
}
- (void)mouseDown:(NSEvent *)e
{
	NSPoint			m=[self convertPoint:[e locationInWindow] fromView:nil];
	NSDictionary	*dic;

	if(rindex==0)
		dic=[self hitTimecourseAtPoint:m];
	else
	if(rindex==1)
		dic=[self hitPhaseDiagramAtPoint:m];
	
	if(dic)
	{
		[msg setStringValue:[NSString stringWithFormat:@"%@-%@",[dic objectForKey:@"sub"],[dic objectForKey:@"age"]]];
		[LData setSindex:[[dic objectForKey:@"i"] intValue]];
		[app selectTableRowAtIndex:[LData sindex]];
		[self setNeedsDisplay:YES];
	}
	else
		[LData setSindex:-1];
}
-(void)mouseDragged:(NSEvent*)e
{
	NSPoint			m=[self convertPoint:[e locationInWindow] fromView:nil];
	
	if([LData sindex]<0)
		return;
	
	if(rindex==0)
	{
		NSRect			r=[self frame];
		float			x,y;
		NSMutableArray	*age=[LData age];
		NSMutableArray	*var=[[[LData var] objectAtIndex:vindex] objectForKey:@"vals"];
		
		x=m.x*(tmax-tmin)/r.size.width+tmin;
		y=m.y*(vmax-vmin)/r.size.height+vmin;
		
		[age replaceObjectAtIndex:[LData sindex] withObject:[NSString stringWithFormat:@"%g",x]];
		[var replaceObjectAtIndex:[LData sindex] withObject:[NSString stringWithFormat:@"%g",y]];
	}
	else
	if(rindex==1)
		[self hitPhaseDiagramAtPoint:m];
	
	[app updateDisplay];
}
#pragma mark -
-(IBAction)changeRepresentation:(id)sender
{
	int	rep=[sender indexOfSelectedItem];
	[self setRepresentationIndex:rep];
	[self setNeedsDisplay:YES];
}
-(IBAction)changeVariable:(id)sender
{
	int	var=[sender indexOfSelectedItem];
	[self setVariableIndex:var];
	[self setNeedsDisplay:YES];
}
-(IBAction)changeGroup:(id)sender
{
	int	grp=[sender indexOfSelectedItem];
	[self setGroupIndex:grp];
	[self setNeedsDisplay:YES];
}
-(IBAction)changeTransform:(id)sender
{
	int	tr=[sender indexOfSelectedItem];
	[self setTransformIndex:tr];
	[self setNeedsDisplay:YES];
}
-(void)setVariableIndex:(int)theVindex
{
	vindex=theVindex;
	
	NSDictionary	*dic;
	dic=[LData findLimitsForVariableAtIndex:vindex];
	vmin=[[dic objectForKey:@"min"] floatValue];
	vmax=[[dic objectForKey:@"max"] floatValue];
	dic=[LData findLimitsForVariableDerivateAtIndex:vindex];
	dvmin=[[dic objectForKey:@"min"] floatValue];
	dvmax=[[dic objectForKey:@"max"] floatValue];
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
#pragma mark - 
-(void)setData:(LongitudinalData*)theLData
{
	LData=theLData;

	// configure pop-ups
	int	i;
	for(i=0;i<[[LData var] count];i++)
	{
		[popVariable addItemWithTitle:[[[LData var] objectAtIndex:i] objectForKey:@"name"]];
		[popGroup addItemWithTitle:[[[LData var] objectAtIndex:i] objectForKey:@"name"]];
	}
	
	NSDictionary	*dic;
	dic=[LData findLimitsForAge];
	tmin=[[dic objectForKey:@"min"] floatValue];
	tmax=[[dic objectForKey:@"max"] floatValue];
}
-(void)setApp:(id)theApp
{
	app=theApp;
}
#pragma mark -
-(void)drawData
{
	NSRect			r=[self frame];
	int				i,j,N;
	float			x,y;
	NSArray			*sub=[LData sub];
	NSArray			*age=[LData age];
	NSArray			*var=[[[LData var] objectAtIndex:vindex] objectForKey:@"vals"];
	NSMutableArray	*raw=[NSMutableArray arrayWithCapacity:10];
	
	j=-1;
	N=[age count];
	for(i=0;i<N;i++)
	{
		x=[[age objectAtIndex:i] floatValue];
		y=[[var objectAtIndex:i] floatValue];
		[raw addObject:NSStringFromPoint((NSPoint){x,y})];
		
		if(i==[LData sindex])			// pick up the selected point
			j=[raw count]-1;
		
		if(i==N-1 || ([[sub objectAtIndex:i] isEqualTo:[sub objectAtIndex:i+1]]==NO))
		{
			[[[LData subcolors] objectForKey:[sub objectAtIndex:i]] set];
			
			if(tindex==1)
				[LData fitVonBertalanffy:raw];
			else
			if(tindex==2)
				[LData fitSpline:raw];
			else
			if(tindex==3)
				[LData fitGompertz:raw];
			else
			if(tindex==4)
				[LData fitLogistic:raw];
			else
			if(tindex==5)
				[LData fitHenri:raw];
			else
			if(tindex==6)
				[LData fitAsymptotic:raw];
			
			if(rindex==0)
				[self drawTimecourse:raw selectedIndex:j atRect:r];
			else
			if(rindex==1)
				[self drawPhaseDiagram:raw selectedIndex:j atRect:r];
			
			raw=[NSMutableArray arrayWithCapacity:10];
			j=-1;
		}
	}
}
-(void)drawTimecourse:(NSMutableArray*)raw selectedIndex:(int)j atRect:(NSRect)r
{
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
-(void)drawPhaseDiagram:(NSMutableArray*)raw selectedIndex:(int)j atRect:(NSRect)r
{
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
	NSArray			*sub=[LData sub];
	NSArray			*age=[LData age];
	NSArray			*var=[[[LData var] objectAtIndex:vindex] objectForKey:@"vals"];
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
	NSArray			*sub=[LData sub];
	NSArray			*age=[LData age];
	NSArray			*var=[[[LData var] objectAtIndex:vindex] objectForKey:@"vals"];
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
@end
