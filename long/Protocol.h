@protocol MyDocument
 -(NSArray*)cmds;
 -(void)applyCmd:(NSArray*)theCmd;
 -(void)selectTableRowAtIndex:(int)i;
 -(void)updateDisplay;
 @end