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
		data=nil;
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

	[view1 setApp:self];
	[view2 setApp:self];
	[text setApp:self];
	[self initCmds];	
	[[NSNotificationCenter defaultCenter]	addObserver:self
											 selector:@selector(printString:)
												 name:@"MyPrintString"
											   object:nil];
	[text insertText:@"> "];
	
	[self readData];
	[view1 setData:data];
	[view1 setVariableIndex:1];
	[view1 setNeedsDisplay:YES];
	[view2 setData:data];
	[view2 setVariableIndex:1];
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
	
	sub=[data objectForKey:@"Subject"];
	age=[data objectForKey:@"Age"];
	vars=[data objectForKey:@"Variables"];
	
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
	
	sub=[data objectForKey:@"Subject"];
	age=[data objectForKey:@"Age"];
	vars=[data objectForKey:@"Variables"];
	
	[sub removeObjectAtIndex:row];
	[age removeObjectAtIndex:row];
	
	for(i=0;i<[vars count];i++)
	{
		val=[[vars objectAtIndex:i] objectForKey:@"vals"];
		[val removeObjectAtIndex:row];
	}
	[table reloadData];
}
-(void)readData
{
	if([self fileURL]==NULL)
		return;
	
	if(data!=NULL)
		[data release];
	data=[NSMutableDictionary new];
	
	FILE				*f;
	char				*ptr,str[1024];
	int					i,m,n,ind[2]={-1,-1};
	NSMutableArray		*arr,*var,*sub,*age;
	
	f=fopen([[[self fileURL] path] UTF8String],"r");

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
				n++;
			}
			
			ptr=strtok(NULL," ,\t\n");
			m++;
		}
	}
	while(!feof(f));

	// configure pop-ups
	for(i=0;i<[var count];i++)
	{
		[popVariable addItemWithTitle:[[var objectAtIndex:i] objectForKey:@"name"]];
		[popGroup addItemWithTitle:[[var objectAtIndex:i] objectForKey:@"name"]];
	}
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
	
	arr=[data objectForKey:@"Variables"];
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
	int		nrows=[[data objectForKey:@"Subject"] count];
	NSArray	*arr;
	
	if(rowIndex<0||rowIndex>=nrows)
		return nil;
	
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Subject"])
		val= [[data objectForKey:@"Subject"] objectAtIndex:rowIndex];
	else
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Age"])
		val=[[data objectForKey:@"Age"] objectAtIndex:rowIndex];
	else
	{
		arr=[data objectForKey:@"Variables"];
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
	int		nrows=[[data objectForKey:@"Subject"] count];
	NSArray	*arr;
	
	if(rowIndex<0||rowIndex>=nrows)
		return;
	
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Subject"])
		[[data objectForKey:@"Subject"] replaceObjectAtIndex:rowIndex withObject:anObject];
	else
	if([[[aTableColumn headerCell] stringValue] isEqualTo:@"Age"])
		[[data objectForKey:@"Age"] replaceObjectAtIndex:rowIndex withObject:anObject];
	else
	{
		arr=[data objectForKey:@"Variables"];
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
	return [[data objectForKey:@"Subject"] count];
}
#pragma mark -
-(void)initCmds
{
	NSString	*cmdstr=@"{root=(\
{cmd=commands;													help=\"List all commands\";},\
{cmd=help;				args=(string);							help=\"Help\";},\
{cmd=laplace;			args=(string,string,int);				help=\"Laplace transform for s=[0,smax]. arg[1]=variable, arg[2]=subject, arg[3]=smax\";},\
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
	[text insertText:[dic objectForKey:@"string"]];
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
			invoc=[NSInvocation invocationWithMethodSignature:[view1 methodSignatureForSelector:sel]];
			[invoc setSelector:sel];
			[invoc setTarget:view1];
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
