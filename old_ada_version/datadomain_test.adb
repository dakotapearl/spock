with DataDomain; use DataDomain;
with Ada.Text_Io; use Ada.Text_Io;

procedure DataDomain_Test is
   
begin
   for i in -1..1 loop
      Put_Line("To_String of Integer("&integer'image(i)&"): '" & Data_ToString(DD_Integer(i).all) & "'");
   end loop;
   for i in -2..2 loop
      Put_Line("To_String of Integer("&integer'image(i)&"): '" & Data_ToString(DD_Integer(i).all) & "'");
   end loop;
   
   Put_Line("Data_ToString(DD_Integer_Range(min => 2, max => 4)) = " & Data_ToString(DD_Integer_Range(min => 2, max => 4)));
   
   if isDataRange(DD_Integer_Range(min => 2, max => 4)) then
      Put_Line("DD_Integer_Range(min => 2, max => 4) is a data range! Woohoo!");
   else
      Put_Line("DD_Integer_Range(min => 2, max => 4) isn't quite a data range yet :(");
   end if;

   if isDataRange(DD_Integer(7)) /= true then
      Put_Line("DD_Integer(7) isn't a data range! Woohoo!");
   else
      Put_Line("DD_Integer(7) isn't quite not a data range yet :(");
   end if;

   if getDataType(DD_Integer(3)) = DT_Integer then
      Put_Line("DD_Integer(3) is of type DT_Integer!");
   else
      Put_Line("DD_Integer(3) isn't quite of type DT_Integer :(");
   end if;
   
   if getDataType(DD_Integer_Range(min => 2, max => 4)) = DT_Integer then
      Put_Line("DD_Integer_Range(min => 2, max => 4) is of type DT_Integer!");
   else   
      Put_Line("DD_Integer_Range(min => 2, max => 4) isn't quite of type DT_Integer :(");
   end if;
   
   -- Data Data
   if inOrEqualTo(Left => DD_Integer(3), Right => DD_Integer(3)) then
      Put_Line("1 passed");
   else
      Put_Line("1 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer(3), Right => DD_Integer(4)) /= true then
      Put_Line("2 passed");
   else
      Put_Line("2 failed");
   end if;
   
   -- Data Range
   if inOrEqualTo(Left => DD_Integer(3), Right => DD_Integer_Range(min => 2, max => 4)) then
      Put_Line("3 passed");
   else
      Put_Line("3 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer(4), Right => DD_Integer_Range(min => 2, max => 4)) then
      Put_Line("4 passed");
   else
      Put_Line("4 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer(5), Right => DD_Integer_Range(min => 2, max => 4)) /= true then
      Put_Line("5 passed");
   else
      Put_Line("5 failed");
   end if;
   
   -- Range Data
   if inOrEqualTo(Left => DD_Integer_Range(min => 2, max => 4), Right => DD_Integer(5)) /= true then
      Put_Line("6 passed");
   else
      Put_Line("6 failed");
   end if;
   
   -- Range Range
   if inOrEqualTo(Left => DD_Integer_Range(min => 2, max => 4), 
                  Right => DD_Integer_Range(min => 2, max => 4)) then
      Put_Line("7 passed");
   else
      Put_Line("7 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer_Range(min => 1, max => 4), 
                  Right => DD_Integer_Range(min => 2, max => 4)) /= true then
      Put_Line("8 passed");
   else
      Put_Line("8 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer_Range(min => 3, max => 4), 
                  Right => DD_Integer_Range(min => 2, max => 4)) then
      Put_Line("9 passed");
   else
      Put_Line("9 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer_Range(min => 2, max => 5), 
                  Right => DD_Integer_Range(min => 2, max => 4)) /= true then
      Put_Line("10 passed");
   else
      Put_Line("10 failed");
   end if;
   if inOrEqualTo(Left => DD_Integer_Range(min => 2, max => 3), 
                  Right => DD_Integer_Range(min => 2, max => 4)) then
      Put_Line("11 passed");
   else
      Put_Line("11 failed");
   end if;

   -- Incompatible
   if inOrEqualTo(Left => DD_Integer_Range(min => 2, max => 4), 
                  Right => DD_Blank) /= true then
      Put_Line("12 passed");
   else
      Put_Line("12 failed");
   end if;

   
   Put_Line("Character data type tests");
   
   Put_Line("Data_ToString(DD_Character('A')) = " & Data_ToString(DD_Character('A')));
   Put_Line("Data_ToString(DD_Character('b')) = " & Data_ToString(DD_Character('b')));
   Put_Line("Data_ToString(DD_Character('+')) = " & Data_ToString(DD_Character('+')));
   Put_Line("Data_ToString(DD_Character_Range('L', 'P')) = " & Data_ToString(DD_Character_Range('L', 'P')));


   Put_Line("Exponential data type tests");
   
   for i in 0..9 loop
      Put_Line("Data_ToString(DD_Exponential("&integer'image(i)&")) = " & Data_ToString(DD_Exponential(i)));
   end loop;
   
   Put_Line("Boolean data type tests");
   Put_Line("Data_ToString(DD_Boolean(true)) = " & Data_ToString(DD_Boolean(true)));
   Put_Line("Data_ToString(DD_Boolean(false)) = " & Data_ToString(DD_Boolean(false)));
   Put_Line("Data_ToString(DD_Boolean_Both) = " & Data_ToString(DD_Boolean_Both));

   Put_Line("Blank data type test");
   Put_Line("Data_ToString(DD_Blank) = " & Data_ToString(DD_Blank));
   
   Put_Line("");Put_Line("Printing whole data pot");
   DataPotPrint;
end DataDomain_Test;
