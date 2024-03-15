with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Main is

-- EXPRC DEFINITIONS

   type ExprC is abstract tagged null record;
   -- abstract : cannot be accessed directly
   -- tagged : enable inheritance from ExprC. Basically
   --    allows for creation of derivative types such as NumC
   --    which stil have the type ExprC (polymorphism)
   -- null record : empty struct
   type ExprC_Access is access all ExprC'Class;
   -- access : access type, which is basically a pointer
   -- all : can read and write to memory that the pointer points to
   -- 'Class : Class wide type covering ExprC and all it's derivates

   type NumC is new ExprC with record
      N : Integer;
   end record;

   type String_Access is access String;
   -- access : Keep as a pointer to dynamically allocate memory on the heap.
   --    If didn't use access the memory would be allcoated on the stack and
   --    we would have to mention a fixed size and the
   type StringC is new ExprC with record
      S : String_Access;
   end record;

   type IdC is new ExprC with record
      S : String_Access;
   end record;

   type IfC is new ExprC with record
      If_Expr   : ExprC_Access;
      Then_Expr : ExprC_Access;
      Else_Expr : ExprC_Access;
   end record;

   type AppC is new ExprC with record
      Fun   : ExprC_Access;
      Left  : ExprC_Access;
      Right : ExprC_Access;
   end record;

-- VALUE DEFINITIONS
   type Value is abstract tagged null record;
   type Value_Access is access all Value'Class;

   type Real_Value is new Value with record
      Data : Integer;
   end record;

   type String_Value is new Value with record
      Data : String_Access;
   end record;

   type Bool_Value is new Value with record
      Data : Boolean;
   end record;

-- PRIMITIVE OPERATIONS
   function Prim_Plus(Left, Right : Value'Class) return Value'Class is
   L : Real_Value;
   R : Real_Value;
   begin
      L := Real_Value(Left);
      R := Real_Value(Right);
      return Real_Value'(Data => L.Data + R.Data);
   exception
      when others =>
         raise Constraint_Error with "Prim_Plus: operands must be Real_Value";
   end Prim_Plus;

   function Prim_Minus(Left, Right : Value'Class) return Value'Class is
   L : Real_Value;
   R : Real_Value;
   begin
      L := Real_Value(Left);
      R := Real_Value(Right);
      return Real_Value'(Data => L.Data - R.Data);
   exception
      when others =>
         raise Constraint_Error with "Prim_Minus: operands must be Real_Value";
   end Prim_Minus;

   function Prim_Divide(Left, Right : Value'Class) return Value'Class is
   L : Real_Value;
   R : Real_Value;
   begin
      L := Real_Value(Left);
      R := Real_Value(Right);
      return Real_Value'(Data => L.Data / R.Data);
   exception
      when others =>
         raise Constraint_Error with "Prim_Divide: operands must be Real_Value";
   end Prim_Divide;

   function Prim_Multiply(Left, Right : Value'Class) return Value'Class is
   L : Real_Value;
   R : Real_Value;
   begin
      L := Real_Value(Left);
      R := Real_Value(Right);
      return Real_Value'(Data => L.Data * R.Data);
   exception
      when others =>
         raise Constraint_Error with "Prim_Multiply: operands must be Real_Value";
   end Prim_Multiply;

   function Prim_Le(Left, Right : Value'Class) return Value'Class is
      L : Real_Value;
      R : Real_Value;
   begin
      L := Real_Value(Left);
      R := Real_Value(Right);
      return Bool_Value'(Data => L.Data <= R.Data);
   exception
      when others =>
         raise Constraint_Error with "Prim_Le: operands must be Real_Value";
   end Prim_Le;

   function Prim_Eq(Left, Right : Value'Class) return Value'Class is
      L : Real_Value;
      R : Real_Value;
   begin
      L := Real_Value(Left);
      R := Real_Value(Right);
      return Bool_Value'(Data => L.Data = R.Data);
   exception
      when others =>
         raise Constraint_Error with "Prim_Eq: operands must be Real_Value";
   end Prim_Eq;

   type Prim_Op_Access is access function (Left, Right : Value'Class) return Value'Class;
   type Prim_Value is new Value with record
      Op : Prim_Op_Access;
   end record;

-- ENVIRONMENT

   type Binding;
   type Binding_Access is access Binding;

   type Binding is record
      Name : String_Access;
      Val  : Value_Access;
      Next : Binding_Access := null;
   end record;

   type Environment is record
      Head : Binding_Access := null;
   end record;

   procedure Extend_Env(Env : in out Environment; Name : String_Access; Val : Value_Access) is
      New_Binding : Binding_Access;
   begin
      New_Binding := new Binding'(Name, Val, Env.Head);
      Env.Head := New_Binding;
   end Extend_Env;

   function Lookup(Name : String; Env : Environment) return Value'Class is
      Current : Binding_Access := Env.Head;
   begin
      while Current /= null loop
         if Current.Name.all = Name then
               return Current.Val.all;
         end if;
         Current := Current.Next;
      end loop;
      raise Program_Error with "Lookup: Could not find name in environment";
   end Lookup;

   function Create_Initialized_Env return Environment is
      New_Env : Environment;
   begin
      Extend_Env(New_Env, new String'("true"), new Bool_Value'(Data => True));
      Extend_Env(New_Env, new String'("false"), new Bool_Value'(Data => False));
      Extend_Env(New_Env, new String'("+"), new Prim_Value'(Op => Prim_Plus'Access));
      Extend_Env(New_Env, new String'("-"), new Prim_Value'(Op => Prim_Minus'Access));
      Extend_Env(New_Env, new String'("/"), new Prim_Value'(Op => Prim_Divide'Access));
      Extend_Env(New_Env, new String'("*"), new Prim_Value'(Op => Prim_Multiply'Access));
      Extend_Env(New_Env, new String'("<="), new Prim_Value'(Op => Prim_Le'Access));
      Extend_Env(New_Env, new String'("equal?"), new Prim_Value'(Op => Prim_Eq'Access));
      return New_Env;
   end Create_Initialized_Env;

   Top_Env : Environment := Create_Initialized_Env;

-- INTERP
   function Interp(Expr : ExprC'Class; Env : Environment) return Value'Class is
   begin
      if Expr in NumC'Class then
         declare
            Num : NumC := NumC(Expr);
         begin
            return Real_Value'(Data => Num.N);
         end;
      elsif Expr in StringC'Class then
         declare
            Str : StringC := StringC(Expr);
         begin
            return String_Value'(Data => Str.S);
         end;
      elsif Expr in IdC'Class then
         declare
            Id : IdC := IdC(Expr);
         begin
            return Lookup(Id.S.all, Env);
         end;
      elsif Expr in IfC'Class then
      declare
         If_Expr : IfC := IfC(Expr);
         Condition_Value : Value'Class := Interp(If_Expr.If_Expr.all, Env);
      begin
         if Condition_Value in Bool_Value'Class then
            if Bool_Value(Condition_Value).Data then
               return Interp(If_Expr.Then_Expr.all, Env);
            else
               return Interp(If_Expr.Else_Expr.all, Env);
            end if;
         else
            raise Program_Error with "If test does not produce a boolean";
         end if;
      end;
      elsif Expr in AppC'Class then
      declare
         App        : constant AppC := AppC(Expr);
         Fun_Id     : constant IdC := IdC(App.Fun.all);
         Operation  : constant Value'Class := Lookup(Fun_Id.S.all, Env);
         Left_Val   : constant Value'Class := Interp(App.Left.all, Env);
         Right_Val  : constant Value'Class := Interp(App.Right.all, Env);
      begin
         if Operation in Prim_Value'Class then
            return Prim_Value(Operation).Op.all(Left_Val, Right_Val);
         else
            raise Program_Error with "Function in AppC is not a valid operation";
         end if;
      end;

      else
         raise Program_Error with "Unsupported expression type";
      end if;
   end Interp;


-- TESTS
   A : Real_Value := (Data => 10);
   B : Real_Value := (Data => 5);
   C : NumC := (N => 10);
   D : StringC := (S => new String'("hi"));
   E : IfC := (If_Expr => new IdC'((S => new String'("true"))), 
      Then_Expr => new IdC'((S => new String'("x"))), 
      Else_Expr => new IdC'((S => new String'("y"))));
   F : IfC := (If_Expr => new IdC'((S => new String'("false"))), 
      Then_Expr => new IdC'((S => new String'("x"))), 
      Else_Expr => new IdC'((S => new String'("y"))));
   G : AppC := (Fun => new IdC'((S => new String'("+"))), 
      Left => new IdC'((S => new String'("x"))), 
      Right => new IdC'((S => new String'("y"))));

   begin
      Ada.Text_IO.Put_Line("10 + 5 = " & Integer'Image(Real_Value(Prim_Plus(A, B)).Data));
      Ada.Text_IO.Put_Line("10 - 5 = " & Integer'Image(Real_Value(Prim_Minus(A, B)).Data));
      Ada.Text_IO.Put_Line("10 * 5 = " & Integer'Image(Real_Value(Prim_Multiply(A, B)).Data));
      Ada.Text_IO.Put_Line("10 / 5 = " & Integer'Image(Real_Value(Prim_Divide(A, B)).Data));
      Ada.Text_IO.Put_Line("10 <= 5 = " & Boolean'Image(Bool_Value(Prim_Le(A, B)).Data));
      Ada.Text_IO.Put_Line("10 = 5 = " & Boolean'Image(Bool_Value(Prim_Eq(A, B)).Data));
      Ada.Text_IO.Put_Line("5 = 5 = " & Boolean'Image(Bool_Value(Prim_Eq(B, B)).Data));
      Ada.Text_IO.Put_Line("(interp (NumC 1)) = " & Integer'Image(Real_Value(Interp(C, Top_Env)).Data));
      Ada.Text_IO.Put_Line("(interp (StringC hi)) = " & String_Value(Interp(D, Top_Env)).Data.all);

      Extend_Env(Top_Env, new String'("x"), new Real_Value'(Data => 9));
      Extend_Env(Top_Env, new String'("y"), new Real_Value'(Data => 2));

      Ada.Text_IO.Put_Line("(interp (IfC true x y)) = " & Integer'Image(Real_Value(Interp(E, Top_Env)).Data));
      Ada.Text_IO.Put_Line("(interp (IfC false x y)) = " & Integer'Image(Real_Value(Interp(F, Top_Env)).Data));
      Ada.Text_IO.Put_Line("(interp (AppC + x y)) = " & Integer'Image(Real_Value(Interp(G, Top_Env)).Data));

   exception
   when E : others =>
      Ada.Text_IO.Put_Line("An error occurred: " & Ada.Exceptions.Exception_Message(E));
end Main;
