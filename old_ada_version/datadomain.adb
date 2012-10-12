with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body DataDomain is

   function getDataType (argData : Data_Pointer) return DataType is
   begin
      return getDataType_private (argData.all);
   end getDataType;
   
   function isDataRange (argData : Data_Pointer) return Boolean is
   begin
      return isDataRange_private (argData.all);
   end isDataRange;

   -- returns false if types are not compatible or for two ranges
   function inOrEqualTo (Left, Right : in not null Data_Pointer) return Boolean is
   begin
      if getDataType(Left) = getDataType(Right) then
         if isDataRange(Right) then
            if isDataRange(Left) then
               -- Range Range
               if Range_LowerValue(Left.all) > Range_LowerValue(Right.all) or Range_LowerValue(Left.all) = Range_LowerValue(Right.all) then
                  if Range_UpperValue(Left.all) < Range_UpperValue(Right.all) or Range_UpperValue(Left.all) = Range_UpperValue(Right.all) then
                     --Put_Line("inOrEqualTo: Range Range true");
                     return true;
                  else
                     --Put_Line("inOrEqualTo: Range Range false 1");
                     return false;
                  end if;
               else
                  --Put_Line("inOrEqualTo: Range Range false 2");
                  return false;
               end if;
            else
               -- Data Range
               if Left > Range_LowerValue(Right.all) or Left = Range_LowerValue(Right.all) then
                  if Left < Range_UpperValue(Right.all) or Left = Range_UpperValue(Right.all) then
                     --Put_Line("inOrEqualTo: Data Range true");
                     return true;
                  else
                     --Put_Line("inOrEqualTo: Data Range false 1");
                     return false;
                  end if;
               else
                  --Put_Line("inOrEqualTo: Data Range false 2");
                  return false;
               end if;
            end if;
         else
            if isDataRange(Left) then
               -- Range Data
               --Put_Line("inOrEqualTo: Range Data");
               return false;
            else
               -- Data Data
               --Put_Line("inOrEqualTo: Data Data");
               return Left.all = Right.all;
            end if;
         end if;
      else
         --Put_Line("inOrEqualTo: Incompatible data types");
         return false;
      end if;
   end inOrEqualTo;

   function GenerateRandomValue (RangeOfData : Data_Pointer) return Data_Pointer is
      dt : DataType;
      result : Data_Pointer;
   begin
      if isDataRange(RangeOfData) then
         dt := getDataType(RangeOfData);
         if dt = DT_Integer then
            result := Random_Integer(getInteger(Range_LowerValue(RangeOfData.all)),getInteger(Range_UpperValue(RangeOfData.all)));
         else
            result := DD_Blank;
         end if;
         return result;
      else
         Put_Line("DataDomain.GenerateRandomValue error: Data pointer not a data range.");      
         return DD_Blank;
      end if;
   end GenerateRandomValue;

   
   function DD_Integer (content : Integer) return Data_Pointer is
      newPtr : IntegerCell_Pointer;
      id : Unbounded_String;
   begin
      id := To_Unbounded_String("DD_Integer" & Integer'Image(content));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new IntegerCell'(id => newDataID, Data => content);
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Integer;
   
   function DD_Integer_Range (min : Integer; max : Integer) return Data_Pointer is
      newPtr : IntegerRange_Pointer;
      tempMax : Integer;
      tempMin : Integer;
      id : Unbounded_String;
   begin
      if min < max then
         tempMax := max;
         tempMin := min;
      elsif min > max then
         tempMax := min;
         tempMin := max;
      elsif min = max then
         return DD_Integer(min);
      end if;
      id := To_Unbounded_String("DD_Integer_Range" & Integer'Image(tempMin) & Integer'Image(tempMax));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new IntegerRange'(id => newDataID, min => DD_Integer(tempMin), max => DD_Integer(tempMax));
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Integer_Range;

   function getInteger (Data : Data_Pointer) return Integer is
   
      function getInteger_subfunction (DataObject : DataCell'Class) return Integer is
      begin
         if DataObject'tag = IntegerCell'tag then
            return IntegerCell(DataObject).data;
         else
            Put_Line("DataDomain.getInteger: Not an integer data cell! Check it before you wreck it!");
            return -999999;
         end if;
      end getInt;
      
   begin
      return getInteger_subfunction(Data.all);
   end getInteger;

   function DD_Character (content : Character) return Data_Pointer is
      newPtr : CharacterCell_Pointer;
      id : Unbounded_String;
   begin
      id := To_Unbounded_String("DD_Character" & Character'Image(content));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new CharacterCell'(id => newDataID, Data => content);
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Character;
   
   function DD_Character_Range (min : Character; max : Character) return Data_Pointer is
      newPtr : CharacterRange_Pointer;
      tempMax : Character;
      tempMin : Character;
      id : Unbounded_String;
   begin
      if min < max then
         tempMax := max;
         tempMin := min;
      elsif min > max then
         tempMax := min;
         tempMin := max;
      elsif min = max then
         return DD_Character(min);
      end if;
      id := To_Unbounded_String("DD_Character_Range" & Character'Image(tempMin) & Character'Image(tempMax));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new CharacterRange'(id => newDataID, min => DD_Character(tempMin), max => DD_Character(tempMax));
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Character_Range;
   
   function DD_Exponential (power : Natural) return Data_Pointer is
      newPtr : ExponentialCell_Pointer;
      id : Unbounded_String;
   begin
      id := To_Unbounded_String("DD_Exponential" & Natural'Image(power));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new ExponentialCell'(id => newDataID, Data => power);
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Exponential;
   
   function DD_Exponential_Range (min_power : Natural; max_power : Natural) return Data_Pointer is
      newPtr : ExponentialRange_Pointer;
      tempMax : Natural;
      tempMin : Natural;
      id : Unbounded_String;
   begin
      if min_power < max_power then
         tempMax := max_power;
         tempMin := min_power;
      elsif min_power > max_power then
         tempMax := min_power;
         tempMin := max_power;
      elsif min_power = max_power then
         return DD_Exponential(min_power);
      end if;
      id := To_Unbounded_String("DD_Exponential_Range" & Natural'Image(tempMin) & Natural'Image(tempMax));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new ExponentialRange'(id => newDataID, min => DD_Exponential(tempMin), max => DD_Exponential(tempMax));
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Exponential_Range;
   
   function DD_Boolean (content : Boolean) return Data_Pointer is
      newPtr : BooleanCell_Pointer;
      id : Unbounded_String;
   begin
      id := To_Unbounded_String("DD_Boolean" & Boolean'Image(content));
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new BooleanCell'(id => newDataID, Data => content);
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Boolean;
   
   function DD_Boolean_Both return Data_Pointer is
      newPtr : BooleanRange_Pointer;
      id : Unbounded_String;
   begin
      id := To_Unbounded_String("DD_Boolean_Range'false''true'");
      if DataPotPackage.Find(DataPot, id) /= DataPotPackage.No_Element then
         return DataPot.Element(id);
      else
         newPtr := new BooleanRange'(id => newDataID, min => DD_Boolean(false), max => DD_Boolean(true));
         DataPot.Insert(id, DataCell(newPtr.all)'Access);
         return DataCell(newPtr.all)'Access;
      end if;
   end DD_Boolean_Both;

   function DD_Blank return Data_Pointer is
   begin
      if Blank_ptr = null then
         TempBlankptr := new BlankCell'(id => newDataID);
         Blank_ptr := DataCell(TempBlankptr.all)'Access;
      end if;
      return Blank_ptr;
   end DD_Blank;
      
   function Data_ToString (argData : in DataCell'Class) return String is
   begin
      if argData in DataRange'Class then
         return "(" & Data_ToString(DataRange(argData).min.all) & ".." & Data_ToString(DataRange(argData).max.all) & ")";
      elsif argData'tag = IntegerCell'tag then
         return Trim(Integer'Image(IntegerCell(argData).data), Ada.Strings.Left);
      elsif argData'tag = CharacterCell'tag then
         return "" & CharacterCell(argData).data;
      elsif argData'tag = ExponentialCell'tag then
         return Trim(Integer'Image(10 ** ExponentialCell(argData).data), Ada.Strings.Left);
      elsif argData'tag = BooleanCell'tag then
         if BooleanCell(argData).Data = true then
            return "True";
         else
            return "False";
         end if;
      elsif argData'tag = BlankCell'tag then
         return "<Null Data>";
      else
         return "Failed, unknown data type.";
      end if;
   end Data_ToString;

   function Data_ToString (argData : in Data_Pointer) return String is
   begin
      return Data_ToString (argData.all);
   end Data_ToString;
   
   function "=" (Left, Right : in DataCell) return Boolean is
   begin
      return Integer(Left.id) = Integer(Right.id);
   end "=";
      
   function "<" (Left, Right : in Data_Pointer) return Boolean is
   begin
      return lessthan_private(Left.all, Right.all);
   end "<";
   
   function ">" (Left, Right : in Data_Pointer) return Boolean is
   begin
      return greaterthan_private(Left.all, Right.all);
   end ">";

   function newDataID return DataID is
   begin
      DataIDNewCursor := DataIDNewCursor + 1;
      return DataIDNewCursor - 1;
   end newDataID;
   
   function getDataType_private (Data : DataCell'Class) return DataType is
   begin
      if data'tag = IntegerCell'tag or data'tag = IntegerRange'tag then
         return DT_Integer;
      elsif data'tag = CharacterCell'tag then
         return DT_Character;
      elsif data'tag = ExponentialCell'tag then
         return DT_Exponential;
      elsif data'tag = BooleanCell'tag then
         return DT_Boolean;
      elsif data'tag = BlankCell'tag then
         return DT_Blank;
      end if;
      return DT_Unknown;
   end getDataType_private;
   
   function isDataRange_private (Data : DataCell'Class) return Boolean is
   begin
      if Data in DataRange'Class then
         return true;
      else
         return false;   
      end if;   
   end isDataRange_private;

   function lessthan_private (Left, Right : DataCell'Class) return Boolean is
   begin
      if Left'tag = IntegerCell'tag and Right'tag = IntegerCell'tag then
         return IntegerCell(Left).data < IntegerCell(Right).data;
      elsif Left'tag = CharacterCell'tag and Right'tag = CharacterCell'tag then
         return CharacterCell(Left).data < CharacterCell(Right).data;
      elsif Left'tag = ExponentialCell'tag and Right'tag = ExponentialCell'tag then
         return ExponentialCell(Left).data < ExponentialCell(Right).data;
      elsif Left'tag = BooleanCell'tag and Right'tag = BooleanCell'tag then
         if BooleanCell(Left).data /= true and BooleanCell(Right).data = true then
            return true;
         else
            return false;
         end if;
      else
         return false;
      end if;
   end lessthan_private;
   
   function greaterthan_private (Left, Right : DataCell'Class) return Boolean is
   begin
      if Left'tag = IntegerCell'tag and Right'tag = IntegerCell'tag then
         return IntegerCell(Left).data > IntegerCell(Right).data;
      elsif Left'tag = CharacterCell'tag and Right'tag = CharacterCell'tag then
         return CharacterCell(Left).data > CharacterCell(Right).data;
      elsif Left'tag = ExponentialCell'tag and Right'tag = ExponentialCell'tag then
         return ExponentialCell(Left).data > ExponentialCell(Right).data;
      elsif Left'tag = BooleanCell'tag and Right'tag = BooleanCell'tag then
         if BooleanCell(Left).data = true and BooleanCell(Right).data /= true then
            return true;
         else
            return false;
         end if;
      else
         return false;
      end if;
   end greaterthan_private;
   
   function Range_LowerValue (RangeOfData : DataCell'Class) return Data_Pointer is
   begin
      if RangeOfData in DataRange'Class then
         return DataRange(RangeOfData).min;
      else
         return DD_Blank;
      end if;
   end Range_LowerValue;
   
   function Range_UpperValue (RangeOfData : DataCell'Class) return Data_Pointer is
   begin
      if RangeOfData in DataRange'Class then
         return DataRange(RangeOfData).max;
      else
         return DD_Blank;
      end if;
   end Range_UpperValue;

   procedure DataPotPrint is
   
      procedure PrintPot(C: in DataPotPackage.Cursor) is
      begin
         Put_Line(Data_ToString(DataPotPackage.Element(C)));
      end PrintPot;
   
   begin
      DataPot.Iterate(PrintPot'Access);
   end DataPotPrint;
   
   function "<" (Left, Right : in DataID) return Boolean is
   begin
      return Integer(Left) < Integer(Right);
   end "<";
   
   function getDataID (Data : in Data_Pointer) return DataID is
   begin
      return Data.all.id;
   end getDataID;
   
   function Random_Integer (Lower_Limit : Natural; Upper_Limit : Natural) return Natural is
      result : Natural;
      l, u: Float;
   begin
      if Lower_Limit <= Upper_Limit then
         l := Float(Integer(Lower_Limit));
         u := Float(Integer(Upper_Limit)) + 1.0;
         u := Random(G) * (u - l) + l; --can consolodate
         result := Natural(Integer(Float'Floor(u))); --these two
         if result > Upper_Limit then
            result := result - 1;
         end if;
      else
         Put_Line("Warning: Lower limit is greater than upper limit. 99999 Returned.");
         result := 99999;
      end if;
      return result;
   end Random_Integer;

   
end DataDomain;
