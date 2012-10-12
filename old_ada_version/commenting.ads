package Commenting is

   type CommentVerbosity is (None, StartupOnly, Additional);
   c1_comments : Boolean := false; -- Message Passes
   c2_comments : Boolean := false; -- Network node comments
   c3_comments : Boolean := false; -- Initialisations
   c4_comments : Boolean := false; -- Network management interface comments
   c5_comments : Boolean := false; -- Genetics testings
   cv : CommentVerbosity := StartupOnly;
   procedure SetCommentVerbosity (argcv : CommentVerbosity);
   procedure c1 (comment : String); 
   procedure c2 (comment : String);
   procedure c3 (comment : String);
   procedure c4 (comment : String);
   procedure c5 (comment : String);

end Commenting;
