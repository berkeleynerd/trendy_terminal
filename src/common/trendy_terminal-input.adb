with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Trendy_Terminal.VT100;

package body Trendy_Terminal.Input is
    package ASU renames Ada.Strings.Unbounded;
    use all type ASU.Unbounded_String;
    function "+"(S : String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;

    function Length(Self : in Line) return Natural is (Current(Self)'Length);

    procedure Move_Cursor (Self : in out Line; Direction : Cursor_Direction) is
    begin
        case Direction is
            when Left =>
                if Self.Cursor > 1 then
                    Self.Cursor := Self.Cursor - 1;
                    VT100.Cursor_Left;
                end if;
            when Right =>
                if Self.Cursor <= ASU.Length (Self.Contents) then

                    Self.Cursor := Self.Cursor + 1;
                    VT100.Cursor_Right;
                end if;
        end case;
    end Move_Cursor;

    function Cursor_Index (Self : in Line) return Positive is
    begin
        return Self.Cursor;
    end Cursor_Index;

    procedure Insert (Self : in out Line; S : String) is
    begin
        ASU.Insert(Self.Contents, Self.Cursor, S);
        Self.Cursor := Self.Cursor + S'Length;
    end Insert;

    procedure Backspace (Self : in out Line) is
    begin
        if Self.Cursor = 1 then
            return;
        end if;
        ASU.Delete(Self.Contents, Self.Cursor - 1, Self.Cursor - 1);
        Move_Cursor(Self, Left);
    end Backspace;

    procedure Delete (Self : in out Line) is
    begin
        if ASU.Length (Self.Contents) > 0 and then Self.Cursor <= ASU.Length(Self.Contents) then
            ASU.Delete (Self.Contents, Self.Cursor, Self.Cursor);

            if Self.Cursor > ASU.Length (Self.Contents) + 1 then
                Move_Cursor (Self, Left);
            end if;
        end if;
    end Delete;

    procedure Clear (Self : in out Line) is
    begin
        Self.Cursor := 1;
        Self.Contents := ASU.Null_Unbounded_String;
    end Clear;

    function Current (Self : Line) return String is (ASU.To_String(Self.Contents));
end Trendy_Terminal.Input;
