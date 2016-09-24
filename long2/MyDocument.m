//
//  MyDocument.m
//  long
//
//  Created by roberto on 25/11/2010.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import "MyDocument.h"

@implementation MyDocument

- (id)init
{
    self = [super init];
    if (self)
	{
		LData=nil;
    }
    return self;
}

- (NSString *)windowNibName
{
    // Override returning the nib file name of the document
    // If you need to use a subclass of NSWindowController or if your document supports multiple NSWindowControllers, you should remove this method and override -makeWindowControllers instead.
    return @"MyDocument";
}

- (void)windowControllerDidLoadNib:(NSWindowController *) aController
{
    [super windowControllerDidLoadNib:aController];

	if([self fileURL]==NULL)
		return;
	
	LData=[LongitudinalData new];
	[LData setApp:self];
	[LData readData:[self fileURL]];
	[view1 setData:LData];
	[view2 setData:LData];
	[view1 setApp:self];
	[view2 setApp:self];
	[text setApp:self];
	[view1 setVariableIndex:0];
	[view2 setVariableIndex:0];
	
	[self initCmds];	
	[[NSNotificationCenter defaultCenter]	addObserver:self
											 selector:@selector(printString:)
												 name:@"MyPrintString"
											   object:nil];
	[text insertText:@"> "];
	
	[view1 setNeedsDisplay:YES];
	[view2 setNeedsDisplay:YES];
	
	[self initTable];
}

- (NSData *)dataOfType:(NSString *)typeName error:(NSError **)outError
{
    // Insert code here to write your document to data of the specified type. If the given outError != NULL, ensure that you set *outError when returning nil.

    // You can also choose to override -fileWrapperOfType:error:, -writeToURL:ofType:error:, or -writeToURL:ofType:forSaveOperation:originalContentsURL:error: instead.

    // For applications targeted for Panther or earlier systems, you should use the deprecated API -dataRepresentationOfType:. In this case you can also choose to override -fileWrapperRepresentationOfType: or -writeToFile:ofType: instead.

    if ( outError != NULL ) {
		*outError = [NSError errorWithDomain:NSOSStatusErrorDomain code:unimpErr userInfo:NULL];
	}
	return nil;
}

- (BOOL)readFromURL:(NSURL *)url ofType:(NSString *)typeName error:(NSError **)outError
{
    if ( outError != NULL ) {
		*outError = [NSError errorWithDomain:NSOSStatusErrorDomain code:unimpErr userInfo:NULL];
	}
    return YES;
}
#pragma mark -
-(IBAction)addRow:(id)sender
{
	NSMutableArray	*sub;
	NSMutableArray	*age;
	NSArray			*vars;
	NSMutableArray	*val;
	int				i;
	int				row=[table selectedRow];
	
	sub=[LData sub];
	age=[LData age];
	vars=[LData var];
	
	[sub insertObject:[NSString stringWithString:@" "] atIndex:row];
	[age insertObject:[NSString stringWithString:@" "] atIndex:row];
	
	for(i=0;i<[vars count];i++)
	{
		val=[[vars objectAtIndex:i] objectForKey:@"vals"];
		[val insertObject:[NSString stringWithString:@" "] atIndex:row];
	}
	[table reloadData];
}
-(IBAction)deleteRow:(id)sender
{
	NSMutableArray	*sub;
	NSMutableArray	*age;
	NSArray			*vars;
	NSMutableArray	*val;
	int				i;
	int				row=[table selectedRow];
	
	sub=[LData sub];
	age=[LData age];
	vars=[LData var];
	
	[sub removeObjectAtIndex:row];
	[age removeObjectAtIndex:row];
	
	for(i=0;i<[vars count];i++)
	{
		val=[[vars objectAtIndex:i] objectForKey:@"vals"];
		[val removeObjectAtIndex:row];
	}
	[table reloadData];
}
#pragma mark -
-(void)initTable
{
	printf("Init table\n");
	
	NSTableColumn	*col;
	NSArray			*arr;
	int				i;
	NSString		*key;

	// Add columns and data rows to table
	col=[[NSTableColumn alloc] initWithIdentifier:@"Subject"];
	[[col headerCell] setStringValue:@"Subject"];
	[table addTableColumn:col];
	[col release];

	col=[[NSTableColumn alloc] initWithIdentifier:@"Age"];
	[[col headerCell] setStringValue:@"Age"];
	[table addTableColumn:col];
	[col release];
	
	arr=[LData var];
	for(i=0;i<[arr count];i++)
	{
		key=[[arr objectAtIndex:i] objectForKey:@"name"];
		col=[[NSTableColumn alloc] initWithIdentifier:key];
		[[col headerCell] setStringValue:key];
		[table addTableColumn:col];
		[col release];
	}
	[table reloadData];	
}
-(void)selectTableRowAtIndex:(int)i
{
	[table selectRowIndexes:[NSIndexSet indexSetWithIndex:i] byExtendingSelection:NO];
}
-(id)tableView:(NSTableView *)aTableView objectValueForTableColumn:(NSTableColumn *)aTableColumn row:(int)rowIndex
{
	int		i;
	id		val=nil;
	int		nrows=[[LData sub] count];
	NSArray	*arr;
	
	if(rowIndex<0||rowIndex>=nrows)
		return nil;
	
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Subject"])
		val= [[LData sub] objectAtIndex:rowIndex];
	else
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Age"])
		val=[[LData age] objectAtIndex:rowIndex];
	else
	{
		arr=[LData var];
		for(i=0;i<[arr count];i++)
			if([[[aTableColumn headerCell] stringValue] isEqualTo:[[arr objectAtIndex:i] objectForKey:@"name"]])
				break;
		if(i==[arr count])
			return nil;
		val=[[[arr objectAtIndex:i] objectForKey:@"vals"] objectAtIndex:rowIndex];
	}

	return val;
}
- (void)tableView:(NSTableView *)aTableView setObjectValue:(id)anObject forTableColumn:(NSTableColumn *)aTableColumn row:(NSInteger)rowIndex
{
	int		i;
	int		nrows=[[LData sub] count];
	NSArray	*arr;
	
	if(rowIndex<0||rowIndex>=nrows)
		return;
	
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Subject"])
		[[LData sub] replaceObjectAtIndex:rowIndex withObject:anObject];
	else
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Age"])
		[[LData age] replaceObjectAtIndex:rowIndex withObject:anObject];
	else
	{
		arr=[LData var];
		for(i=0;i<[arr count];i++)
			if([[[aTableColumn headerCell] stringValue] isEqualTo:[[arr objectAtIndex:i] objectForKey:@"name"]])
				break;
		if(i==[arr count])
			return;
		[[[arr objectAtIndex:i] objectForKey:@"vals"] replaceObjectAtIndex:rowIndex withObject:anObject];
	}

}
-(int)numberOfRowsInTableView:(NSTableView*)aTableView
{
	return [[LData sub] count];
}
#pragma mark -
-(void)initCmds
{
	NSString	*cmdstr=@"{root=(\
{cmd=commands;													help=\"List all commands\";},\
{cmd=fit;				args=(string,string);					help=\"Fit model to data, 1 subject at the time. Arg[1]=Variable name, arg[2]={asymp,asymp2,spline,vonbertalanffy,gompertz,logistic}\";},\
{cmd=predicted;			args=(string,string);					help=\"Fit model to data, 1 subject at the time, display the predicted values. Arg[1]=Variable name, arg[2]={asymp,asymp2,spline,vonbertalanffy,gompertz,logistic,henri}\";},\
{cmd=fitall;			args=(string,string);					help=\"Fit model to data, all subjects together. Arg[1]=Variable name, arg[2]={spline,vonbertalanffy,gompertz,logistic,henri}\";},\
{cmd=fitplot;			args=(string,string,string,string);		help=\"Fit model to data. Arg[1]=Variable name, arg[2]={spline,vonbertalanffy,gompertz,logistic},arg[3] = plot limits in the format 'xmin xmax ymin ymax', arg[4]={time,phase}\";},\
{cmd=help;				args=(string);							help=\"Help\";},\
{cmd=laplace;			args=(string,string,int);				help=\"Laplace transform for s=[0,smax]. arg[1]=variable, arg[2]=subject, arg[3]=smax\";},\
{cmd=plot;				args=(string,string,string);			help=\"Plot data. Arg[1]=Variable name, arg[2]='xmin xmax ymin ymax'=plot limits, arg[3]={time,phase}\";},\
{cmd=subjects;													help=\"List all subjects\";},\
{cmd=variables;													help=\"List all variables\";},\
);}";
	NSDictionary	*dic=[cmdstr propertyListFromStringsFileFormat];
	cmds=[[dic objectForKey:@"root"] retain];
}
-(NSArray*)cmds
{
	return cmds;
}
-(void)printString:(NSNotification*)n
{
	//if([n object]!=view) return;
	NSDictionary	*dic=[n userInfo];
	[text insertText:@"\n"];
	[text insertText:[dic objectForKey:@"string"]];
	[text insertText:@"\n"];
}
-(void)applyCmd:(NSArray*)theCmd
{
	int			i,m;
	int			a;
	float		x;
	char		*s;
	NSInvocation	*invoc;
	
	for(i=0;i<[cmds count];i++)
	{
		if([[theCmd objectAtIndex:0] caseInsensitiveCompare:[[cmds objectAtIndex:i] objectForKey:@"cmd"]]==NSOrderedSame)
		{
			NSString		*cmd=[[cmds objectAtIndex:i] objectForKey:@"cmd"];
			NSArray			*args=[[cmds objectAtIndex:i] objectForKey:@"args"];
			NSMutableString	*sig=[NSMutableString stringWithString:cmd];
			for(m=0;m<[args count];m++)
				[sig appendString:@":"];
			SEL sel=NSSelectorFromString(sig);
			invoc=[NSInvocation invocationWithMethodSignature:[LData methodSignatureForSelector:sel]];
			[invoc setSelector:sel];
			[invoc setTarget:LData];
			for(m=0;m<[args count];m++)
			{
				if([[args objectAtIndex:m] isEqualTo:@"int"])
				{
					a=[[theCmd objectAtIndex:m+1] intValue];
					[invoc setArgument:&a atIndex:m+2];
				}
				else
					if([[args objectAtIndex:m] isEqualTo:@"float"])
					{
						x=[[theCmd objectAtIndex:m+1] floatValue];
						[invoc setArgument:&x atIndex:m+2];
					}
					else
						if([[args objectAtIndex:m] isEqualTo:@"string"])
						{
							s=(char*)[[theCmd objectAtIndex:m+1] UTF8String];
							[invoc setArgument:&s atIndex:m+2];
						}
			}
			[invoc invoke];
			[view1 setNeedsDisplay:YES];
			[view2 setNeedsDisplay:YES];
			return;
		}
	}
	if(i==[cmds count])
		[text insertText:@"\nCommand unknown\n"];
}

-(void)updateDisplay
{
	[view1 setNeedsDisplay:YES];
	[view2 setNeedsDisplay:YES];
}
@end
