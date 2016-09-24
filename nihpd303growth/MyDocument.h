//
//  MyDocument.h
//  nihpd303growth
//
//  Created by roberto on 25/01/2011.
//  Copyright 2011 __MyCompanyName__. All rights reserved.
//


#import <Cocoa/Cocoa.h>
#import "MyView.h"

@interface MyDocument : NSDocument
{
	IBOutlet MyView	*view;
}
@end
