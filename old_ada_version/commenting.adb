with Ada.Text_Io; use Ada.Text_Io;

package body Commenting is

   procedure SetCommentVerbosity (argcv : CommentVerbosity) is
   begin
      cv := argcv;
   end SetCommentVerbosity;
   
   procedure c1 (comment : String) is
   begin
      if cv = Additional and c1_comments = true then
         Put_Line("C1 (Message passes): " & comment);
      end if;
   end c1;
   
   procedure c2 (comment : String) is
   begin
      if cv = Additional and c2_comments = true then
         Put_Line("C2 (NetworkNode): " & comment);
      end if;
   end c2;
   
   procedure c3 (comment : String) is
   begin
      if cv = Additional and c3_comments = true then
         Put_Line("C3 (Inits): " & comment);
      end if;
   end c3;

   procedure c4 (comment : String) is
   begin
      if cv = Additional and c4_comments = true then
         Put_Line("C4 (Network Interfaces): " & comment);
      end if;
   end c4;

   procedure c5 (comment : String) is
   begin
      if cv = Additional and c5_comments = true then
         Put_Line("C5 (Genetics): " & comment);
      end if;
   end c5;

end Commenting;
