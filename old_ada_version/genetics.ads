with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with DataDomain; use DataDomain;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Genetics is
   
   type GeneticCode is limited private;
   type GeneticCode_Pointer is access all GeneticCode;
   type PropertyID is new Unbounded_String;
   type PropertyID_Pointer is access all PropertyID;

   -- template structure
   type TemplateProperty;
   type TemplateProperty_Pointer is access all TemplateProperty;
   type TemplatePropertyArray is array (Integer range <>) of TemplateProperty_Pointer;
   type TemplatePropertyArray_Pointer is access all TemplatePropertyArray;
   type PropertyValuesArray is array (Integer range <>) of Data_Pointer;
   type PropertyValuesArray_Pointer is access all PropertyValuesArray;
   type TemplateProperty is tagged record
      id : PropertyID;
      DefaultValue : Data_Pointer := DD_Blank;
      AcceptableValues : Data_Pointer := null;
      Children : TemplatePropertyArray_Pointer := null;
   end record;
   type TemplateCode is record 
      root : TemplateProperty_Pointer;
   end record;
   type TemplateCode_Pointer is access all TemplateCode;
   
   procedure SetAcceptableValues (argProperty : in out not null TemplateProperty_Pointer; argValues : in Data_Pointer);
   procedure SetChildren (argProperty : in out not null TemplateProperty_Pointer; argCount : in Natural);
   procedure SetName (argProperty : in out not null TemplateProperty_Pointer; argName : in String);
   procedure SetValue (argProperty : in out not null TemplateProperty_Pointer; argValue : in Data_Pointer);
   function newTemplateCode return TemplateCode_Pointer;
   
   type percentage is new float range 0.0 .. 1.0;
   procedure Initialise (argTemplate : in TemplateCode_Pointer);
   function mutate (gc : GeneticCode_Pointer; mutationRate : percentage) return GeneticCode_Pointer;
   function splice (gc1 : GeneticCode_Pointer; gc2 : GeneticCode_Pointer) return GeneticCode_Pointer;
   procedure setProperty (gc : in out GeneticCode_Pointer; id : in String; value : in Data_Pointer);
   function getProperty (gc : GeneticCode_Pointer; id : String) return Data_Pointer;
   function newGeneticCode return GeneticCode_Pointer;
      
   function TemplateCode_ToString (tc : not null TemplateCode_Pointer) return String;
   function Code_ToString (gc : not null GeneticCode_Pointer) return String;
   
  
   
   function GeneticsIsInitialised return boolean;
   
private
   
   type CodeProperty;
   type CodeProperty_Pointer is access all CodeProperty;
   type CodePropertyArray is array (Integer range <>) of CodeProperty_Pointer;
   type CodePropertyArray_Pointer is access all CodePropertyArray;
   type CodeProperty is tagged record
      id : PropertyID;
      Value : Data_Pointer := DD_Blank;
      AcceptableValues : Data_Pointer := null;
      Children : CodePropertyArray_Pointer := null;
   end record;

   function getPropertyID (argProperty : CodeProperty_Pointer) return String;
   
   package PropertyMap is new Ada.Containers.Ordered_Maps (Element_Type => CodeProperty_Pointer,
                                                           Key_Type => Unbounded_String);
   type PropertyMap_Pointer is access all PropertyMap.Map;

   type GeneticCode is record
      root : CodeProperty_Pointer;
      map : PropertyMap_Pointer;
   end record;   
   
   Template : TemplateCode_Pointer := null;
   procedure DeterminePropertyCount (tc : TemplateCode);
   PropertyCount : Natural;
   
end Genetics;
