with Ada.Text_Io; use Ada.Text_Io;
with Commenting; use Commenting;
with genetics; use genetics;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with DataDomain; use DataDomain;

procedure genetics_test is
   gc : GeneticCode_Pointer;
   gc2 : GeneticCode_Pointer;
   tc : TemplateCode_Pointer;
   Props : PropertyValuesArray_Pointer;
begin
   
   SetCommentVerbosity(Additional);
   c5_comments := false;
   
   InitialiseDD;
   
   --TODO list
   --  dynamics lists
   --  faster way to check if data is acceptable via maps or sets

   Put_Line("Checking template integrity checks with faulty template");

   -- Integrity check with faulty template
   tc := newTemplateCode;
   
   SetName(tc.root, "root node");
   SetChildren(tc.root, 7);
   
   Props := new PropertyValuesArray(1..8);
   Props(1) := DD_Character('A');
   Props(2) := DD_Character('B');
   
   SetName(tc.root.Children(1), "First");
   SetValue(tc.root.Children(1), DD_Character('A'));
   SetAcceptableValues(tc.root.Children(1), Props);
   
   SetName(tc.root.Children(2), "Second");
   SetValue(tc.root.Children(2), DD_Character('C')); --(1) Data value outside range
   SetAcceptableValues(tc.root.Children(2), Props);

   SetName(tc.root.Children(3), "Third");
   SetAcceptableValues(tc.root.Children(3), Props);
   SetChildren(tc.root.Children(3), 1); --(2) Unset child

   --SetName(tc.root.Children(4), "Forth"); --(3) Name not set
   SetValue(tc.root.Children(4), DD_Character('A'));
   SetAcceptableValues(tc.root.Children(4), Props);

   SetName(tc.root.Children(5), "Fifth");
   SetValue(tc.root.Children(5), DD_Character('B')); --(4) Children and defaultvalue both set
   SetAcceptableValues(tc.root.Children(5), Props);
   SetChildren(tc.root.Children(5), 1);
   SetName(tc.root.Children(5).Children(1), "Fifth point First");
   SetValue(tc.root.Children(5).Children(1), DD_Character('B'));
   SetAcceptableValues(tc.root.Children(5).Children(1), Props);

   SetName(tc.root.Children(6), "First"); --(5) repeated id
   SetValue(tc.root.Children(6), DD_Character('A'));
   SetAcceptableValues(tc.root.Children(6), Props);

   SetName(tc.root.Children(7), "Seventh");
   SetValue(tc.root.Children(7), DD_Character('A'));
   --SetAcceptableValues(tc.root.Children(7), Props); --(6) AcceptableValues not set while DefaultValue is
   
   -- Attempt initialisation
   Initialise(tc);
   
   
   
   Put_Line("Checking functionality with complete template");
   
   -- Build complete template for functionality test
   tc := newTemplateCode;
   
   SetName(tc.root, "root node");
   SetChildren(tc.root, 4);
   
   Props := new PropertyValuesArray(1..8);
   Props(1) := DD_Character('A');
   Props(2) := DD_Character('B');
   Props(3) := DD_Character('C');
   Props(4) := DD_Character('D');
   Props(5) := DD_Character('W');
   Props(6) := DD_Character('X');
   Props(7) := DD_Character('Y');
   Props(8) := DD_Character('Z');
   
   SetName(tc.root.Children(1), "1. First");
   SetValue(tc.root.Children(1), DD_Character('A'));
   SetAcceptableValues(tc.root.Children(1), Props);
   
   SetName(tc.root.Children(2), "2. Second");
   SetValue(tc.root.Children(2), DD_Character('B'));
   SetAcceptableValues(tc.root.Children(2), Props);

   SetName(tc.root.Children(3), "SubParent");
   SetChildren(tc.root.Children(3), 2);
   
   SetName(tc.root.Children(3).Children(1), "3.1. First child");
   SetValue(tc.root.Children(3).Children(1), DD_Character('C'));
   SetAcceptableValues(tc.root.Children(3).Children(1), Props);
   
   SetName(tc.root.Children(3).Children(2), "3.2. Second child");
   SetValue(tc.root.Children(3).Children(2), DD_Character('D'));
   SetAcceptableValues(tc.root.Children(3).Children(2), Props);
   
   SetName(tc.root.Children(4), "SubParent2");
   SetChildren(tc.root.Children(4), 5);

   Props := new PropertyValuesArray(0..9);
   for i in Props'Range loop
      Props(i) := DD_Integer(i);
   end loop;
   
   for i in 0..4 loop
      SetName(tc.root.Children(4).Children(i+1), Integer'Image(i));
      SetValue(tc.root.Children(4).Children(i+1), DD_Integer(i));
      SetAcceptableValues(tc.root.Children(4).Children(i+1), Props);
   end loop;
   
   Put_Line("Template code: " & TemplateCode_ToString(tc));
   
   Initialise(tc);
   
   
   gc := newGeneticCode;
   
   Put_Line("Genetic code: " & Code_ToString(gc));
   
   Put_Line("Set property test:");
   
   setProperty(gc => gc,
               id => "2. Second",
               value => DD_Character('X'));
   
   Put_Line("Modifed code: " & Code_ToString(gc));
   
   Put_Line("Get individual value:");
   
   Put_Line("'1. First' = " & Data_ToString(getProperty(gc => gc, id => "1. First").all));
   Put_Line("'2. Second' = " & Data_ToString(getProperty(gc => gc, id => "2. Second").all));
   Put_Line("'3.1. First child' = " & Data_ToString(getProperty(gc => gc, id => "3.1. First child").all));
   Put_Line("'3.2. Second child' = " & Data_ToString(getProperty(gc => gc, id => "3.2. Second child").all));
   
   Put_Line("Mutation test (x5) @ mutate rate 50%");
   for i in 1..5 loop
      gc := newGeneticCode;
      gc := mutate(gc, percentage(0.5));
      Put_Line(Code_ToString(gc));
   end loop;
   
   Put_Line("Splice test (x5)");
   for i in 1..5 loop
      gc := newGeneticCode;
      gc2 := newGeneticCode;
      
      -- change gc2
      setProperty(gc2, "1. First", DD_Character('W'));
      setProperty(gc2, "2. Second", DD_Character('X'));
      setProperty(gc2, "3.1. First child", DD_Character('Y'));
      setProperty(gc2, "3.2. Second child", DD_Character('Z'));
      
      for i in 0..4 loop
         setProperty(gc2, Integer'Image(i), DD_Integer(i+5));
      end loop;
      
      
      Put(Code_ToString(gc) & " & " & Code_ToString(gc2) & " -> ");
      gc := splice(gc, gc2);
      Put_Line(Code_ToString(gc));
   end loop;
end genetics_test;
