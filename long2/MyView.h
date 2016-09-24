//
//  MyView.h
//  long
//
//  Created by roberto on 26/11/2010.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "LongitudinalData.h"

@interface MyView : NSView
{
	IBOutlet NSTextField	*msg;
	IBOutlet NSPopUpButton	*popVariable;
	IBOutlet NSPopUpButton	*popGroup;

	int		rindex;
	int		vindex;
	int		gindex;
	int		tindex;

	float	tmin,tmax;
	float	vmin,vmax;
	float	dvmin,dvmax;
	
	LongitudinalData		*LData;
	id<MyDocument>			app;
}
-(IBAction)changeRepresentation:(id)sender;
-(IBAction)changeVariable:(id)sender;
-(IBAction)changeGroup:(id)sender;
-(IBAction)changeTransform:(id)sender;

-(void)setVariableIndex:(int)theVindex;
-(void)setGroupIndex:(int)theVindex;
-(void)setRepresentationIndex:(int)theRindex;
-(void)setTransformIndex:(int)theTindex;

-(void)setData:(LongitudinalData *)theLData;
-(void)setApp:(id)theApp;

-(void)drawData;
-(void)drawTimecourse:(NSMutableArray*)raw selectedIndex:(int)j atRect:(NSRect)r;
-(void)drawPhaseDiagram:(NSMutableArray*)raw selectedIndex:(int)j atRect:(NSRect)r;
-(NSDictionary*)hitTimecourseAtPoint:(NSPoint)m;
-(NSDictionary*)hitPhaseDiagramAtPoint:(NSPoint)m;

@end
