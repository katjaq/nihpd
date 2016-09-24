//
//  MyDocument.h
//  long
//
//  Created by roberto on 25/11/2010.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//


#import <Cocoa/Cocoa.h>
#import "MyView.h"
#import "MyCommandView.h"

@interface MyDocument : NSDocument
{
	IBOutlet MyView			*view1;
	IBOutlet MyView			*view2;
	IBOutlet MyCommandView	*text;
	IBOutlet NSTableView	*table;
	IBOutlet NSPopUpButton	*popVariable;
	IBOutlet NSPopUpButton	*popGroup;
	NSArray					*cmds;
	
	NSMutableDictionary		*data;

}
#pragma mark -
-(IBAction)addRow:(id)sender;
-(IBAction)deleteRow:(id)sender;
-(void)readData;

-(void)initTable;
-(void)selectTableRowAtIndex:(int)i;

-(void)initCmds;
-(NSArray*)cmds;
-(void)applyCmd:(NSArray*)theCmd;
-(void)printString:(NSNotification*)n;

-(void)updateDisplay;
@end
