//
//  LongitudinalData.h
//  long
//
//  Created by roberto on 18/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "Protocol.h"

@interface LongitudinalData : NSObject
{
	NSMutableDictionary		*data;
	NSMutableDictionary		*subcolors;
	
	int		rindex;
	int		vindex;
	int		gindex;
	int		tindex;
	float	tmin,tmax;
	float	vmin,vmax;
	float	dvmin,dvmax;
	int		sindex;			// index of selected point

	id<MyDocument>	app;
}
-(void)setData:(NSMutableDictionary*)theData;
-(void)setVariableIndex:(int)theVindex;
-(void)setGroupIndex:(int)theVindex;
-(void)setRepresentationIndex:(int)theRindex;
-(void)setTransformIndex:(int)theTindex;
-(void)colorsForSubjects;
-(void)findLimitsForAge;
-(void)findLimitsForVariable;
-(void)findLimitsForVariableDerivate;
-(void)drawData;
-(void)drawTimecourse:(NSMutableArray*)raw selectedIndex:(int)j;
-(void)drawPhaseDiagram:(NSMutableArray*)raw selectedIndex:(int)j;
-(NSDictionary*)hitTimecourseAtPoint:(NSPoint)m;
-(NSDictionary*)hitPhaseDiagramAtPoint:(NSPoint)m;
-(void)fitVonBertalanffy:(NSMutableArray*)raw;
-(void)fitSpline:(NSMutableArray*)raw;

-(void)setApp:(id)theApp;
-(void)displayMessage:(NSString *)msg;
-(void)commands;
-(void)help:(char*)cmd;
-(void)laplace:(char*)var:(char*)sub:(int)smax;
-(void)subjects;
-(void)variables;
@end
