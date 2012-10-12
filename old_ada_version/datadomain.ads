with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Constants; use Constants;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package DataDomain is

   type DataCell is tagged private;
   type Data_Pointer is access all DataCell;
   
   type DataID is private;   
   function "<" (Left, Right : in DataID) return Boolean;
   function getDataID (Data : in Data_Pointer) return DataID;

   type DataType is (DT_Integer, DT_Character, DT_Exponential, DT_Boolean, DT_Blank, DT_Unknown);
   function getDataType (argData : Data_Pointer) return DataType;
   function isDataRange (argData : Data_Pointer) return Boolean;
   
   function inOrEqualTo (Left, Right : in not null Data_Pointer) return Boolean;
   function "=" (Left, Right : in DataCell) return Boolean;
   function "<" (Left, Right : in Data_Pointer) return Boolean;
   function ">" (Left, Right : in Data_Pointer) return Boolean;
   
   function DD_Integer (content : Integer) return Data_Pointer;
   function DD_Integer_Range (min : Integer; max : Integer) return Data_Pointer;
   function getInteger (Data : Data_Pointer) return Integer;
   function DD_Character (content : Character) return Data_Pointer;
   function DD_Character_Range (min : Character; max : Character) return Data_Pointer;
   function DD_Exponential (power : Natural) return Data_Pointer;
   function DD_Exponential_Range (min_power : Natural; max_power : Natural) return Data_Pointer;
   function DD_Boolean (content : Boolean) return Data_Pointer;
   function DD_Boolean_Both return Data_Pointer;
   function DD_Blank return Data_Pointer;
   
   function GenerateRandomValue (RangeOfData : Data_Pointer) return Data_Pointer;
   function Random_Integer (Lower_Limit : Natural; Upper_Limit : Natural) return Natural;
   G : Generator; 
   
   function Data_ToString (argData : in DataCell'Class) return String;
   function Data_ToString (argData : in Data_Pointer) return String;
   
   --testing
   procedure DataPotPrint;
   
private
   
   type DataID is range 1 .. DataID_Limit;
   DataIDNewCursor : DataID := DataID'First;
   function newDataID return DataID;
   
   type DataCell is tagged
      record
         id : DataID;
      end record;
   
   type DataRange is new DataCell with
      record
         min : Data_Pointer;
         max : Data_Pointer;
      end record;
   type DataRange_Pointer is access all DataRange;

   package DataPotPackage is new Ada.Containers.Ordered_Maps (Element_Type => Data_Pointer,
                                                              Key_Type => Unbounded_String);
   use DataPotPackage;
   DataPot : DataPotPackage.map;


   type IntegerCell is new DataCell with
      record
         data : Integer;
      end record;
   type IntegerCell_Pointer is access all IntegerCell;
   type IntegerRange is new DataRange with null record;
   type IntegerRange_Pointer is access all IntegerRange;
   
   type CharacterCell is new DataCell with
      record
         data : Character;
      end record;
   type CharacterCell_Pointer is access all CharacterCell;
   type CharacterRange is new DataRange with null record;
   type CharacterRange_Pointer is access all CharacterRange;

   type ExponentialCell is new DataCell with
      record
         data : Natural;
      end record;
   type ExponentialCell_Pointer is access all ExponentialCell;
   type ExponentialRange is new DataRange with null record;
   type ExponentialRange_Pointer is access all ExponentialRange;
   
   type BooleanCell is new DataCell with
      record
         data : Boolean;
      end record;
   type BooleanCell_Pointer is access all BooleanCell;
   type BooleanRange is new DataRange with null record;
   type BooleanRange_Pointer is access all BooleanRange;
   type BooleanIndex is (true, false);
   type BooleanArray is array (BooleanIndex) of BooleanCell_Pointer;
   
   type BlankCell is new DataCell with
      record
         null;
      end record;
   type BlankCell_Pointer is access all BlankCell;
   TempBlankptr : BlankCell_Pointer;
   Blank_ptr : Data_Pointer := null;

   function getDataType_private (Data : DataCell'Class) return DataType;
   function isDataRange_private (Data : DataCell'Class) return Boolean;
   function lessthan_private (Left, Right : DataCell'Class) return Boolean;
   function greaterthan_private (Left, Right : DataCell'Class) return Boolean;
   function Range_LowerValue (RangeOfData : DataCell'Class) return Data_Pointer;
   function Range_UpperValue (RangeOfData : DataCell'Class) return Data_Pointer;

end DataDomain;
