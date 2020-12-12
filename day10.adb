with Ada.Text_IO;
with Ada.Containers.Vectors;

procedure Day10 is

   subtype Adaptor_T is Natural;

   package Adaptor_Set_T is new Ada.Containers.Vectors
      (Element_Type => Adaptor_T,
       Index_Type   => Positive);

   package Adaptor_Sorter is new Adaptor_Set_T.Generic_Sorting;

   package Been_There_T is new Ada.Containers.Vectors
      (Element_Type => Long_Long_Integer,
       Index_Type   => Positive);

   Adaptor_List : Adaptor_Set_T.Vector;

   Been_There   : Been_There_T.Vector;

   Diff_1 : Natural := 0;
   Diff_2 : Natural := 0;
   Diff_3 : Natural := 0;

   Num_Paths : Long_Long_Integer;

   function Find_Num_Of_Paths
      (Lower : Natural;
       Upper : Natural;
       List  : Adaptor_Set_T.Vector)
       return Long_Long_Integer
   is
      Upper_Lim : Natural;
      Interim_Result : Long_Long_Integer;
      Result : Long_Long_Integer := 0;
   begin

      if Upper > Adaptor_List.Last_Index then
         Upper_Lim := Adaptor_List.Last_Index;
      else
         Upper_Lim := Upper;
      end if;

      if Lower + 1 > Upper_Lim then
         Result := 1;
         return Result;
      end if;

      for I in Lower + 1 .. Upper_Lim loop
         case Adaptor_List.Element (I) - Adaptor_List.Element (Lower) is
            when 1 .. 2 => 
               Result := Result + Find_Num_Of_Paths (I, I + 3, Adaptor_List);
            when 3 => 
               -- If it's a distance of 3, then this is the only possible path,
               -- so we can save the result to use later.
               if Been_There.Element (I) = 0 then
                  Interim_Result := Find_Num_Of_Paths (I, I + 3, Adaptor_List);
                  Been_There_T.Replace_Element 
                     (Container => Been_There,
                      Index => I,
                      New_Item => Interim_Result);
                  Result := Result + Interim_Result;
               else
                  Result := Result + Been_There.Element (I);
               end if;
            when others =>
               null;
         end case;
      end loop;
      return Result;
   end Find_Num_Of_Paths;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Adaptor_Set_T.Append (Adaptor_List, Adaptor_T'Value (Line));
         Been_There.Append (0);
      end;
      exit when Ada.Text_IO.End_Of_File;
   end loop;

   Adaptor_Sorter.Sort (Adaptor_List);

   Adaptor_List.Prepend (0);
   Been_There.Append (0);

   for I in Adaptor_List.First_Index .. Adaptor_List.Last_Index -1 loop
      case Adaptor_List.Element (I + 1) - Adaptor_List.Element (I) is
         when 1 => 
            Diff_1 := Diff_1 + 1;
         when 2 => 
            Diff_2 := Diff_2 + 1;
         when 3 => 
            Diff_3 := Diff_3 + 1;
         when others =>
            Ada.Text_IO.Put_Line ("Difference too large:" & Adaptor_List.Element (I + 1)'Img & " " & Adaptor_List.Element (I)'Img);
      end case;
   end loop;

   Diff_3 := Diff_3 + 1;

   Ada.Text_IO.Put_Line ("Diff 1:" & Diff_1'Img);
   Ada.Text_IO.Put_Line ("Diff 2:" & Diff_2'Img);
   Ada.Text_IO.Put_Line ("Diff 3:" & Diff_3'Img);

   Ada.Text_IO.Put_Line ("1 * 3:" & Natural(Diff_1 * Diff_3)'Img);

   Num_paths := Find_Num_Of_Paths (1, 4, Adaptor_List);
   Ada.Text_IO.Put_Line ("Num paths: " & Num_Paths'Img);


end Day10;
