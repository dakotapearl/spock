with Commenting; use Commenting;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Text_Io; use Ada.Text_Io;
with DataDomain; use DataDomain;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Tags; use Ada.Tags;
with Ada.Containers.Ordered_Sets;

package body Genetics is

   procedure Initialise(argTemplate : in TemplateCode_Pointer) is
      
      package NameSet is new Ada.Containers.Ordered_Sets (Element_Type => PropertyID);
      use NameSet;
      Names : set;
            
      IntegrityCheckSuccess : Boolean := true;
      
      procedure recursiveIntegrityCheck (template : in out templateProperty) is
      begin
         
         -- check that only one of defaultvalue or children are set (4)
         if template.children /= null and template.defaultvalue /= DD_Blank then
            Put_Line("Template error: DefaultValue and Children both set for node : " & To_String(template.id));
            IntegrityCheckSuccess := false;
         end if;
         
         -- name not set (3)
         if To_String(template.id) = "Uninitialised template property" then
            Put_Line("Template error: Property name not set : " & To_String(template.id));
            IntegrityCheckSuccess := false;
         elsif NameSet.Find(Names, template.id) /= NameSet.No_Element then --name is duplicated (5)
            Put_Line("Template error: Property name is repeated : " & To_String(template.id));
            IntegrityCheckSuccess := false;
         else
            NameSet.Insert(Names, template.id);
         end if;
         
         if template.children /= null then
            for p in template.Children'Range loop
               -- check that child is set (2)
               if To_String(template.Children(p).id) = "Uninitialised template property" and 
                     template.Children(p).DefaultValue = DD_Blank and 
                     template.Children(p).Children = null and 
                     template.Children(p).AcceptableValues = null then
                  Put_Line("Template error: Child" & Integer'Image(p) & " of " & To_String(template.id) & " not set");
                  IntegrityCheckSuccess := false;
               else
                  recursiveIntegrityCheck (template.Children(p).all);
               end if;
               
            end loop;
         else
            --acceptablevalues is set (6)
            
            if template.AcceptableValues = null then
               Put_Line("Template error: AcceptableValues for node " & To_String(template.id) & " not set");
               IntegrityCheckSuccess := false;
            elsif template.DefaultValue /= DD_Blank then
               --value is valid (1)
               if inOrEqualTo(template.DefaultValue, template.AcceptableValues) /= true then
                  Put_Line("Template error: DefaultValue not in AcceptableValues of node " & To_String(template.id));
                  IntegrityCheckSuccess := false;
               end if;
            end if;
            
         end if;
      end recursiveIntegrityCheck;   
      
   begin
      
      
      if template = null then
      
         -- TODO check template integrity
         -- check that all ids are set
         -- check that no two ids are the same
         -- check that only one of defaultvalue or children are set
         -- check that acceptablevalues is set for nodes with a defaultvalue
         -- check that defaultvalue is in acceptablevalues
         -- check for unset children
         
         c5("Verifying template integrity...");
         recursiveIntegrityCheck(argTemplate.all.root.all);
         
         if IntegrityCheckSuccess then
            c5("Genetics module initialised with a verified template code");
            Template := argTemplate;
            DeterminePropertyCount (Template.all);
            Reset(G);
         else
            Put_Line("Error: Template not accepted!");
         end if;
         
      else
         Put_Line("Error: tried to use two genetic templates.");
      end if;
   end Initialise;
   
   function mutate (gc : GeneticCode_Pointer; mutationRate : percentage) return GeneticCode_Pointer is
      
      MutatedCode : GeneticCode_Pointer;
      
      procedure recursiveMutate (code : in out codeProperty; mutant : in out codeProperty) is
      begin
         if code.children /= null then
            for p in code.Children'Range loop
               recursiveMutate (code.Children(p).all, mutant.Children(p).all);
            end loop;
         elsif Float(Random(G)) <= Float(mutationRate) then
            mutant.Value := code.AcceptableValues(Random_Integer(code.AcceptableValues'First, code.AcceptableValues'Last)); --TODO
         else
            mutant.Value := code.Value;
         end if;
      end recursiveMutate;
      
   begin
      c5("Mutating code");
      MutatedCode := newGeneticCode;
      recursiveMutate (gc.all.root.all, MutatedCode.all.root.all);
      
      return MutatedCode;
   end mutate;
      
   -- must splice sub string, not just cut mid-way and swap. Must wrap around so that all values have equally likely chance to be changed.
   function splice (gc1 : GeneticCode_Pointer; gc2 : GeneticCode_Pointer) return GeneticCode_Pointer is
      
      SplicedCode : GeneticCode_Pointer;
      
      --Splices second code into first at a rate of 50%
      procedure recursiveSplice (FirstCode : in out codeProperty; SecondCode : in out codeProperty; SplicedCode : in out codeProperty) is
      begin
         if FirstCode.children /= null then
            for p in FirstCode.Children'Range loop
               recursiveSplice (FirstCode.Children(p).all, SecondCode.Children(p).all, SplicedCode.Children(p).all);
            end loop;
         elsif Random(G) < 0.5 then
            SplicedCode.Value := FirstCode.Value;
         else
            SplicedCode.Value := SecondCode.Value;
         end if;
      end recursiveSplice;
      
   begin
      c5("Splicing code");
      SplicedCode := newGeneticCode;
      recursiveSplice (gc1.all.root.all, gc2.all.root.all, SplicedCode.all.root.all);
      
      return SplicedCode;
   end splice;
   
   
   procedure setProperty (gc : in out GeneticCode_Pointer; id : in String; value : in Data_Pointer) is
   begin
      c5("Setting property");
      gc.map.Element(To_Unbounded_String(id)).Value := value;
   end setProperty;
   
   function getProperty (gc : GeneticCode_Pointer; id : String) return Data_Pointer is
   begin
      c5("Getting property");
      return gc.map.Element(To_Unbounded_String(id)).Value;
   end getProperty;
   
   function newGeneticCode return GeneticCode_Pointer is
      
      newCode : GeneticCode_Pointer;
      
      procedure recursiveBuildCode (template : in TemplateProperty; code : in out codeProperty; codeptr : in CodeProperty_Pointer; PropertyMap : in not null PropertyMap_Pointer) is
      begin
         c5("Building node: " & To_String(template.id));
         PropertyMap.Insert(Unbounded_String(template.id), codeptr);
         c5("Set value " & Data_ToString(template.DefaultValue.all));
         code.Value := template.DefaultValue;
         code.AcceptableValues := template.AcceptableValues;
         if template.children /= null then
            c5("Set children, count" & Integer'Image(template.Children'Length));
            code.Children := new CodePropertyArray (template.Children'Range);
            for p in code.Children'Range loop
               c5("Setting child node number" & Integer'Image(p));
               code.Children(p) := new CodeProperty'(id => template.Children(p).id, value => DD_Blank, AcceptableValues => null, children => null);
               recursiveBuildCode (template.Children(p).all, code.Children(p).all, code.Children(p), PropertyMap);
            end loop;
         end if;
      end recursiveBuildCode;
      
   begin
      c5("Producing new code");
      newCode := new GeneticCode;
      newCode.root := new CodeProperty'(id => To_Unbounded_String("root"), Value => DD_Blank, AcceptableValues => null, Children => null);
      newCode.map := new PropertyMap.map;
      recursiveBuildCode (template.root.all, newCode.root.all, newCode.root, newCode.map);
      c5("Finished recursive build");
      return newCode;
   end newGeneticCode;
   
   procedure DeterminePropertyCount (tc : TemplateCode) is
   
      procedure recursiveCount (template : in out TemplateProperty; c : in out Natural) is
      begin
         if template.children /= null then
            for p in template.Children'Range loop
               recursiveCount (template.Children(p).all, c);
            end loop;
         else
            c := c + 1;
         end if;
      end recursiveCount;
   
      result : Natural := 0;
   
   begin
      
      recursiveCount(tc.root.all, result);
      PropertyCount := result;
      
   end DeterminePropertyCount;
   
   function TemplateCode_ToString (tc : not null TemplateCode_Pointer) return String is
      
      procedure recursiveReturnValues (template : in TemplateProperty; argResult : in out Unbounded_String) is
      begin
         argResult := argResult & Data_ToString(template.DefaultValue.all); 
         if template.children /= null then
            for p in template.Children'Range loop
               recursiveReturnValues (template.Children(p).all, argResult);
            end loop;
         end if;
      end recursiveReturnValues;
      
      result : Unbounded_String := To_Unbounded_String("");
      
   begin
      
      recursiveReturnValues(tc.all.root.all, result);
      
      return To_String(result);
      
   end TemplateCode_ToString;
   
   function Code_ToString (gc : not null GeneticCode_Pointer) return String is
            
      procedure recursiveReturnValues (code : in CodeProperty; argResult : in out Unbounded_String) is
      begin
         argResult := argResult & Data_ToString(code.Value.all); 
         if code.children /= null then
            for p in code.Children'Range loop
               recursiveReturnValues (code.Children(p).all, argResult);
            end loop;
         end if;
      end recursiveReturnValues;
      
      result : Unbounded_String := To_Unbounded_String("");
      
   begin
      
      recursiveReturnValues(gc.all.root.all, result);
      
      return To_String(result);
   end Code_ToString;

   function getPropertyID (argProperty : CodeProperty_Pointer) return String is
   begin
      return To_String(argProperty.all.id);
   end getPropertyID;

   procedure SetAcceptableValues (argProperty : in out not null TemplateProperty_Pointer; argValues : in Data_Pointer) is
   begin
      c5("Setting AcceptableValues");
      argProperty.AcceptableValues := argValues;
   end SetAcceptableValues;

   procedure SetChildren (argProperty : in out not null TemplateProperty_Pointer; argCount : in Natural) is
   begin
      c5("Setting number of children to" & Integer'Image(Integer(argCount)));
      argProperty.all.Children := new TemplatePropertyArray (1 .. argCount);
      for p in argProperty.all.Children'Range loop
         argProperty.all.Children(p) := new TemplateProperty'(id => To_Unbounded_String("Uninitialised template property"), defaultvalue => DD_Blank, AcceptableValues => null, children => null);
      end loop;
      c5("Finished settings children");
   end SetChildren;

   procedure SetName (argProperty : in out not null TemplateProperty_Pointer; argName : in String) is
      s : unbounded_string := To_Unbounded_String("");
      p : PropertyID;
   begin
      c5("Setting node name to " & argName);
      s := s & argName; --TODO clean
      p := PropertyID(s);
      argProperty.all.id := p;
      
   end SetName;

   procedure SetValue (argProperty : in out not null TemplateProperty_Pointer; argValue : in Data_Pointer) is
   begin
      if argProperty.all.Children /= null then
         Put_Line("Error initialising genetics template: Value and Children properties both set");
      end if;
      argProperty.all.DefaultValue := argValue;
      c5("Setting value of property to " & Data_ToString(argValue.all));
   end SetValue;

   function newTemplateCode return TemplateCode_Pointer is
      newCode : TemplateCode_Pointer;
   begin
      
      c5("Generating new template code");
      
      newCode := new TemplateCode;
      newCode.root := new TemplateProperty'(id => To_Unbounded_String("root"), defaultvalue => DD_Blank, AcceptableValues => null, children => null);
      
      return newCode;
   end newTemplateCode;
   
   function GeneticsIsInitialised return boolean is
   begin
      return Template /= null;
   end GeneticsIsInitialised;
   
end Genetics;
