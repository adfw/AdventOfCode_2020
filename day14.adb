with Ada.Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Integer_Text_IO;

procedure Day14 is

   type Mask_T is record
      Valid : Boolean;
      Value : Boolean;
   end record;

   subtype Bit_Length is Natural range 0 .. 35;

   type Mask_36 is array (Bit_Length) of Mask_T;

   Null_Mask_36 : constant Mask_36 := (others => Mask_T'
      (Valid => False,
       Value => False));

   Mask : Mask_36;

   type Mem_Array_36 is array (Bit_Length) of Boolean;

   Null_Mem_Array_36 : constant Mem_Array_36 := (others => False);

   type Big_Mem_Value_T is mod 2 ** 64;
   subtype Mem_Value_T is Big_mem_Value_T range 0 .. 2 ** 36;

   package Value_IO is new Ada.Text_IO.Modular_IO (Mem_Value_T);

   function Hash (X : Natural) return Ada.Containers.Hash_Type is
           (Ada.Containers.Hash_Type'Mod (X));

   package Memory_T is new Ada.Containers.Hashed_Maps 
      (Element_Type => Mem_Value_T,
       Key_Type     => Natural,
       Hash         => Hash,
       Equivalent_Keys => "=");
   
   Memory : Memory_T.Map;
   Cursor : Memory_T.Cursor;

   Mem_Address : Natural;
   Mem_Value   : Mem_Value_T;
   String_Pos  : Natural;
   Insert_Val  : Big_Mem_Value_T;

   Current_Memory : Mem_Array_36 := Null_Mem_Array_36;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         if Line (1 .. 2) = "ma" then
            -- it's the mask. store it.
            for C in Bit_Length'Range loop
               if Line (Line'Length - C) = 'X' then
                  Mask (C).Valid := False;
               else
                  Mask (C).Valid := True;
                  if Line (Line'Length - C) = '1' then
                     Mask (C).Value := True;
                  else
                     Mask (C).Value := False;
                  end if;
               end if;
            end loop;
         else
            -- memory access
            Insert_Val := 0;
            Ada.Integer_Text_IO.Get
              (From => Line (5 .. Line'Length),
               Item => Mem_Address,
               Last => String_Pos);
            Ada.Text_IO.Put_Line ("Addr" & Mem_Address'Img);
            Value_IO.Get
              (From => Line (String_Pos + 5 .. Line'Length),
               Item => Mem_Value,
               Last => String_Pos);
            Ada.Text_IO.Put_Line ("Val" & Mem_Value'Img);
            for B in Bit_Length'Range loop
               if (2 ** B and Mem_Value) = (2 ** B) then
                  Ada.Text_IO.Put_Line (B'Img);
                  if not Mask(B).Valid then
                     Current_Memory (B) := True;
                     Ada.Text_IO.Put_Line ("SET:" & B'Img);
                     Insert_Val := Insert_Val + 2 ** B;
                  end if;
               end if;
               if Mask(B).Valid and then Mask(B).Value then
                  Current_Memory (B) := True;
                  Ada.Text_IO.Put_Line ("SETM" & B'Img);
                  Insert_Val := Insert_Val + 2 ** B;
               end if;
            end loop;
            Ada.Text_IO.Put_Line ("Final Val:" & Insert_Val'Img);
            if not Memory.Contains (Mem_Address) then
                    Memory.Insert (New_Item => Insert_Val,
                                   Key      => Mem_Address);
            else
                    Memory.Replace (New_Item => Insert_Val,
                                    Key      => Mem_Address);
            end if;

                  
         end if;

      end;

      exit when Ada.Text_IO.End_Of_File;
   end loop;

   Cursor := Memory.First;
   Insert_Val := 0;

   while Memory_T.Has_Element (Cursor) loop
           Ada.Text_IO.Put_Line ("Val:" & Memory_T.Element (Cursor)'Img);
           Insert_Val := Insert_Val + Memory_T.Element (Cursor);
           Cursor := Memory_T.Next (Cursor);
   end loop;
   Ada.Text_IO.Put_Line ("Final Val:" & Insert_Val'Img);

end Day14;
