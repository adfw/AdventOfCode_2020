with Ada.Containers.Vectors;
with Ada.Integer_Text_IO;
with Ada.Text_IO;
with Ada.Strings.Fixed;

procedure Day7 is

   Inner_Bag : Natural;
   Outer_Bag : Natural;

   subtype Bag_Name_T is String (1 .. 20);

   Gold_Bag : constant Bag_Name_T := "shiny gold          ";

   type N_Children_T is new Natural range 0 .. 10;
   subtype N_Children_Size_T is N_Children_T range 1 .. 10;

   type Number_Of_Bags is record
      Bag_Id : Natural;
      Count  : Natural;
   end record;

   type Bag_Child is array (N_Children_Size_T) of Number_Of_Bags;

   No_Children_C : constant Bag_Child :=
     (others => Number_Of_Bags'
        (Bag_Id => 0,
         Count  => 0));

   type Bag is record
      Bag_ID      : Natural;
      N_Children  : N_Children_T;
      Children    : Bag_Child;
      Is_Gold_Bag : Boolean;
   end record;

   Empty_Bag : constant Bag := Bag'
     (Bag_Id      => 0,
      N_Children  => 0,
      Children    => No_Children_C,
      Is_Gold_Bag => False);

   package Bag_Name_Lookup_T is new Ada.Containers.Vectors
      (Element_Type => Bag_Name_T,
       Index_Type   => Positive);

   package Bag_T is new Ada.Containers.Vectors
      (Element_Type => Bag,
       Index_Type   => Positive);


   Bag_Lookup : Bag_Name_Lookup_T.Vector;

   Bag_List : Bag_T.Vector;

   The_Bag : Bag_Name_T;

   Visited_Bags : Bag_T.Vector;

   New_Bag : Bag := Empty_Bag;

   Bag_ID : Bag_Name_Lookup_T.Extended_Index;

   Gold_Count_C : Natural := 0;
   Found : Boolean := False;
   Gold_Bag_Count : Natural := 0;

   procedure Insert_Or_Find_Bag
     (Bag_Lookup : in out Bag_Name_Lookup_T.Vector;
      New_Bag    : in     Bag_Name_T;
      New_Bag_ID :    out Positive)
   is
      New_Bag_Index : Bag_Name_Lookup_T.Extended_Index;
   begin

      New_Bag_Index := Bag_Name_Lookup_T.Find_Index
        (Container => Bag_Lookup,
         Item      => New_Bag);

      if New_Bag_Index = Bag_Name_Lookup_T.No_Index then
         Bag_Name_Lookup_T.Append (Bag_Lookup, The_Bag);
         Bag_T.Append (Bag_List, Empty_Bag);
         New_Bag_ID := Bag_Name_Lookup_T.Last_Index (Bag_Lookup);
      else
         New_Bag_ID := New_Bag_Index;
      end if;

   end Insert_Or_Find_Bag;

   function Check_For_Gold
     (C : Bag_T.Cursor;
      Sum : Boolean := False)
      return Natural
   is
      Child_Bag_Name : Bag;

      Num_Bags : Natural := 0;
   begin
      -- Ada.Text_IO.Put_Line ("Checking bag " & Bag_Name_Lookup_T.Element (Bag_Lookup, Bag_T.Element (C).Bag_ID));

      for I in N_Children_T range 1 .. Bag_T.Element (C).N_Children loop
         Child_Bag_Name := Bag_T.Element
           (Container => Bag_List,
            Index     => Bag_T.Element (C).Children (I).Bag_Id);

         Num_Bags := Num_Bags + Bag_T.Element (C).Children (I).Count;

         -- Ada.Text_IO.Put_Line("Child bag is:" & Bag_Name_Lookup_T.Element (Bag_Lookup, Child_Bag_Name.Bag_ID));
         -- Ada.Text_IO.Put_Line ("Interim Part 2: " & Num_Bags'Img);


         if Bag_Name_Lookup_T.Element (Bag_Lookup, Child_Bag_Name.Bag_ID) = Gold_Bag then

            -- Ada.Text_IO.Put_Line ("Found gold B");
            Found := True;
            Num_Bags := 1;

         elsif Child_Bag_Name.N_Children > 0 then
               if Sum then
                  Num_Bags := Num_Bags + Bag_T.Element (C).Children (I).Count * Check_For_Gold (Bag_T.To_Cursor (Bag_List, Child_Bag_Name.Bag_ID), True);
               else
                  Num_Bags := Check_For_Gold (Bag_T.To_Cursor (Bag_List, Child_Bag_Name.Bag_ID));
               end if;
            end if;

         end loop;

      return Num_Bags;

   end Check_For_Gold;

begin

   loop
      declare
         Line : constant String := Ada.Text_IO.Get_Line;
      begin
         Outer_Bag := Ada.Strings.Fixed.Index
            (Source => Line,
             Pattern => " bags") - 1;

         Ada.Strings.Fixed.Move
            (Source => Line (Line'First .. Outer_Bag),
             Target => The_Bag);

         Insert_Or_Find_Bag
           (Bag_Lookup => Bag_Lookup,
            New_Bag    => The_Bag,
            New_Bag_ID => New_Bag.Bag_ID);

         Outer_Bag := Ada.Strings.Fixed.Index
            (Source => Line,
             Pattern => "no other bags");
         if Outer_Bag /= 0 then
            New_Bag.N_Children := 0;
         else
            Outer_Bag := Ada.Strings.Fixed.Index
              (Source => Line,
               Pattern => "contain ") + 6;
            loop
               Outer_Bag := Outer_Bag + 2;
               New_Bag.N_Children := New_Bag.N_Children + 1;
               Ada.Integer_Text_IO.Get
                 (From => Line (Outer_Bag .. Line'Last),
                  Item => New_Bag.Children (New_Bag.N_Children).Count ,
                  Last => Inner_Bag);
               Outer_Bag := Ada.Strings.Fixed.Index
                 (Source => Line (Inner_Bag .. Line'Last),
                  Pattern => " bag");

               Ada.Strings.Fixed.Move
                 (Source => Line (Inner_Bag + 2 .. Outer_Bag - 1),
                  Target => The_Bag);

               Insert_Or_Find_Bag
                 (Bag_Lookup => Bag_Lookup,
                  New_Bag    => The_Bag,
                  New_Bag_ID => Bag_ID);

               New_Bag.Children (New_Bag.N_Children).Bag_Id := Bag_ID;

               if The_Bag = Gold_Bag then
                  New_Bag.Is_Gold_Bag := True;
               end if;

               Outer_Bag := Ada.Strings.Fixed.Index
                 (Source => Line (Outer_Bag .. Line'Last),
                  Pattern => ", ");

               exit when Outer_Bag = 0;
            end loop;
         end if;

         Bag_T.Replace_Element
           (Container => Bag_List,
            Index     => New_Bag.Bag_ID,
            New_Item  => New_Bag);


         Bag_T.Append (Visited_Bags, New_Bag);
         New_Bag := Empty_Bag;

      end;

      exit when Ada.Text_IO.End_Of_File;

   end loop;

   for B in Bag_T.First_Index (Visited_Bags) .. Bag_T.Last_Index (Visited_Bags) loop
      Gold_Bag_Count := Check_For_Gold ( Bag_T.To_Cursor (Bag_List, B));
      if Found then
         Gold_Count_C := Gold_Count_C + 1;
         Found := False;
      end if;
   end loop;

   Ada.Text_IO.Put_Line ("Part 1: " & Gold_Count_C'Img);

   Gold_Bag_Count := Check_For_Gold ( Bag_T.To_Cursor (Bag_List, Bag_Name_Lookup_T.Find_Index (Container => Bag_Lookup,
                                                                   Item      => Gold_Bag)), True);
   Ada.Text_IO.Put_Line ("Part 2: " & Gold_Bag_Count'Img);



end Day7;
