//
//  MyView.h
//  nihpd303growth
//
//  Created by roberto on 26/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>

float	*data;

@interface MyView : NSView
{
	float	vmin,vmax;
	float	amin,amax;
}
-(void)loadData;
-(void)drawData;
-(void)drawDensity;
@end
