//
//  LongitudinalData.h
//  long
//
//  Created by roberto on 18/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "Protocol.h"

#include "mrqmin.h"
#include "spline.h"
#include "amoeba.h"
#include "zriddr.h"

#include <gsl/gsl_bspline.h>
#include <gsl/gsl_multifit.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_statistics.h>

@interface LongitudinalData : NSObject
{
	NSMutableDictionary		*data;
	NSMutableDictionary		*subcolors;
	
	int		sindex;			// index of selected point

	id<MyDocument>	app;
}
-(void)readData:(NSURL*)filePath;
-(void)initColorsForSubjects;
-(NSDictionary*)findLimitsForAge;
-(NSDictionary*)findLimitsForVariableAtIndex:(int)vindex;
-(NSDictionary*)findLimitsForVariableDerivateAtIndex:(int)vindex;
-(NSDictionary*)fitAsymptotic:(NSMutableArray*)raw;
-(NSDictionary*)fitVonBertalanffy:(NSMutableArray*)raw;
-(NSDictionary*)fitHenri:(NSMutableArray*)raw;
-(NSDictionary*)fitGompertz:(NSMutableArray*)raw;
-(NSDictionary*)fitLogistic:(NSMutableArray*)raw;
-(NSDictionary*)fitSpline:(NSMutableArray*)raw;

-(NSArray*)subjectArray;
-(NSMutableArray*)dataForSubject:(NSString*)name variableIndex:(int)vindex;
-(NSMutableArray*)dataForVariableName:(char*)vname;
-(NSDictionary*)variableAtIndex:(int)vindex;

-(NSMutableArray*)sub;
-(NSMutableArray*)age;
-(NSMutableArray*)var;
-(int)sindex;
-(void)setSindex:(int)theSindex;
-(NSMutableDictionary*)subcolors;

-(CGContextRef)createPDFContextWithRect:(CGRect)rect;
-(void)attachContext:(CGContextRef)pdfContext;
-(void)plotVonBertalanffy:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color;
-(void)plotGompertz:(NSDictionary*)dic context:(CGContextRef)pdfContext rect:(CGRect)rect limits:(float*)lim color:(NSColor*)color;
-(void)plotTimecourse:(NSMutableArray*)raw context:(CGContextRef)pdfContext rect:(CGRect)r limits:(float*)lim color:(NSColor*)color;
-(void)plotPhase:(NSMutableArray*)raw context:(CGContextRef)pdfContext rect:(CGRect)r limits:(float*)lim color:(NSColor*)color;

-(void)setApp:(id)theApp;
-(void)displayMessage:(id)msg;
-(void)commands;
-(void)fit:(char*)vname:(char*)model;
-(void)fitall:(char*)vname:(char*)model;
-(void)fitplot:(char*)vname:(char*)model:(char*)limits:(char*)type;
-(void)help:(char*)cmd;
-(void)laplace:(char*)var:(char*)sub:(int)smax;
-(void)plot:(char*)vname:(char*)limits:(char*)type;
-(void)subjects;
-(void)variables;
@end
