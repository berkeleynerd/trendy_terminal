with Ada.Characters.Latin_1;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with Trendy_Terminal.Input;
with Trendy_Terminal.Maps;
with Trendy_Terminal.VT100;

with Trendy_Terminal.Windows;

package body Trendy_Terminal is
    package AIO renames Ada.Text_IO;
    use all type ASU.Unbounded_String;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    package Win renames Trendy_Terminal.Windows;
    use type Win.BOOL;
    use type Win.HANDLE;

    ---------------------------------------------------------------------------
    -- Original settings
    ---------------------------------------------------------------------------
    Input_Settings, Output_Settings, Error_Settings : Win.DWORD;
    Original_Input_CP, Original_Output_CP           : Win.UINT;

    type Input_Stream is record
        Handle   : Win.HANDLE             := Win.INVALID_HANDLE_VALUE;
        Settings : Win.Console_Input_Mode := Win.To_Console_Input_Mode (0);
    end record;

    type Output_Stream is record
        Handle   : Win.HANDLE              := Win.INVALID_HANDLE_VALUE;
        Settings : Win.Console_Output_Mode := Win.To_Console_Output_Mode (0);
    end record;

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input             : Input_Stream;
    Std_Output, Std_Error : Output_Stream;

    procedure Put(C : Character) is
        S       : constant String := (1 => C);
        C_Array : aliased Interfaces.C.char_array := Interfaces.C.To_C(S);
        Native  : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.To_Chars_Ptr(C_array'Unchecked_Access);
        Written : aliased Win.DWORD;
    begin
        if Win.WriteFile (Std_Output.Handle, Win.LPCVOID(Native), S'Length, Written'Unchecked_Access, 0) = 0 then
            AIO.Put_Line ("Write failed.");
        end if;
        Interfaces.C.Strings.Free(Native);
    end Put;

    procedure Put(S : String) is
        Native : aliased Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String(S);
        Written : aliased Win.DWORD;
    begin
        if Win.WriteFile (Std_Output.Handle, Win.LPCVOID(Native), S'Length, Written'Unchecked_Access, 0) = 0 then
            AIO.Put_Line ("Write failed.");
        end if;
        Interfaces.C.Strings.Free(Native);
    end Put;

    procedure Put (S : ASU.Unbounded_String) is
    begin
        Put (ASU.To_String(S));
    end Put;

    procedure Put_Line(S : String) is
    begin
        Put (S & Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.FF);
    end Put_Line;

    procedure Put_Line (S : ASU.Unbounded_String) is
    begin
        Put_Line (ASU.To_String(S));
    end Put_Line;

    procedure Clear_Line is
    begin
        VT100.Clear_Line;
    end Clear_Line;

    procedure New_Line (Num_Lines : Positive) is
    begin
        AIO.New_Line (Ada.Text_IO.Positive_Count (Num_Lines));
    end New_Line;

    procedure Set_Col (Column : Positive) is
        Cursor_Pos : Cursor_Position := VT100.Get_Cursor_Position;
    begin
        Cursor_Pos.Col := Column;
        VT100.Set_Cursor_Position (Cursor_Pos);
    end Set_Col;

    function Load_Std_Settings return Boolean is
        Input_DWORD, Output_DWORD, Error_DWORD : aliased Win.DWORD;
    begin
        if Win.GetConsoleMode (Std_Input.Handle, Input_DWORD'Unchecked_Access) = 0 then
            return False;
        end if;
        if Win.GetConsoleMode (Std_Output.Handle, Output_DWORD'Unchecked_Access) = 0 then
            return False;
        end if;
        if Win.GetConsoleMode (Std_Error.Handle, Error_DWORD'Unchecked_Access) = 0 then
            return False;
        end if;

        Std_Input.Settings  := Win.To_Console_Input_Mode (Input_DWORD);
        Std_Output.Settings := Win.To_Console_Output_Mode (Output_DWORD);
        Std_Error.Settings  := Win.To_Console_Output_Mode (Error_DWORD);

        return True;
    end Load_Std_Settings;

    function Enable_UTF8 return Boolean is
    begin
        return Win.SetConsoleCP (Win.CP_UTF8) /= 0 and then Win.SetConsoleOutputCP (Win.CP_UTF8) /= 0;
    end Enable_UTF8;

    function Init return Boolean is
    begin
        Std_Output.Handle := Win.GetStdHandle (Win.STD_OUTPUT_HANDLE);
        Std_Input.Handle  := Win.GetStdHandle (Win.STD_INPUT_HANDLE);
        Std_Error.Handle  := Win.GetStdHandle (Win.STD_ERROR_HANDLE);

        if Std_Output.Handle = Win.INVALID_HANDLE_VALUE or else Std_Input.Handle = Win.INVALID_HANDLE_VALUE
            or else Std_Error.Handle = Win.INVALID_HANDLE_VALUE then
            Put_Line ("Unable to get one or more of in/out/err handles.");
            return False;
        end if;

        if not Load_Std_Settings then
            return False;
        end if;

        -- Save the initial settings to be restored later.
        Input_Settings  := Win.To_DWORD (Std_Input.Settings);
        Output_Settings := Win.To_DWORD (Std_Output.Settings);
        Error_Settings  := Win.To_DWORD (Std_Error.Settings);

        Original_Input_CP  := Win.GetConsoleCP;
        Original_Output_CP := Win.GetConsoleOutputCP;

        if not Enable_UTF8 then
            Put_Line ("Unable to set UTF8 code page.");
            return False;
        end if;

        return True;
    end Init;

    procedure Shutdown is
    begin
        if Win.SetConsoleMode (Std_Input.Handle, Input_Settings) = 0
            or else Win.SetConsoleMode (Std_Output.Handle, Output_Settings) = 0
            or else Win.SetConsoleMode (Std_Error.Handle, Error_Settings) = 0 then
            Put_Line ("Unable to restore all terminal settings to originals.");
        end if;

        if Win.SetConsoleCP (Original_Input_CP) = 0 or else Win.SetConsoleOutputCP (Original_Output_CP) = 0 then
            Put_Line ("Unable to restore original terminal code page.");
        end if;
    end Shutdown;

    procedure Apply (Input : Input_Stream) is
    begin
        if Win.SetConsoleMode(Input.Handle, Win.To_DWORD(Input.Settings)) = 0 then
            Put_Line ("Unable to change console modes: ERROR#" & Win.GetLastError'Image);
        end if;
    end Apply;

    procedure Apply (Output : Output_Stream) is
    begin
        if Win.SetConsoleMode(Output.Handle, Win.To_DWORD(Output.Settings)) = 0 then
            Put_Line ("Unable to change console modes: ERROR# " & Win.GetLastError'Image);
        end if;
    end Apply;

    procedure Set (Setting : Input_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Echo =>
                Std_Input.Settings (Win.ENABLE_ECHO_INPUT) := Enabled;
                Apply(Std_Input);
            when Line_Input =>
                Std_Input.Settings (Win.ENABLE_LINE_INPUT) := Enabled;
                Apply(Std_Input);
        end case;
    end Set;

    procedure Set (Setting : Output_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Escape_Sequences =>
                Std_Output.Settings (Win.ENABLE_VIRTUAL_TERMINAL_PROCESSING) := Enabled;
                Std_Error.Settings (Win.ENABLE_VIRTUAL_TERMINAL_PROCESSING)  := Enabled;
                Std_Input.Settings (Win.ENABLE_VIRTUAL_TERMINAL_INPUT)       := Enabled;
                Apply(Std_Input);
                Apply(Std_Output);
                Apply(Std_Error);
        end case;
    end Set;

    -- The input stream might have existing inputs, and it may be necessary to
    -- discard these.  A use case would be clearing the input buffer to get
    -- VT100 sequences.
    procedure Clear_Input_Buffer is
        Buffer_Size  : constant := 1024;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Chars_Read   : aliased Win.DWORD;
        Result       : Win.BOOL;
        use all type Win.DWORD;
    begin
        -- Put something into the buffer to ensure it won't block.
        -- It'd be better to peek than do this, but that might fail on named
        -- pipes for inputs and this is just a simple, but hacky way of doing it.
        VT100.Report_Cursor_Position;
        loop
            -- TODO: Support UTF-8
            Result := Win.ReadConsoleA (
                Std_Input.Handle,
                Win.LPVOID(Interfaces.C.Strings.To_Chars_Ptr(Buffer'Unchecked_Access)),
                Buffer_Size,
                Chars_Read'Unchecked_Access,
                0);
            exit when Chars_Read < Buffer_Size;
        end loop;
        pragma Unreferenced (Result);
    end Clear_Input_Buffer;

    -- Gets an entire input line from one keypress.  E.g. all the characters
    -- received for a controlling keypress, such as an arrow key.
    function Get_Input return String is
        Buffer_Size  : constant := 512;
        Buffer       : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t(Buffer_Size) => Interfaces.C.nul);
        Chars_Read   : aliased Win.DWORD;
        use all type Interfaces.C.size_t;
    begin
        if Win.ReadConsoleA (Std_Input.Handle, Win.LPVOID(Interfaces.C.Strings.To_Chars_Ptr(Buffer'Unchecked_Access)),
                         Buffer_Size, Chars_Read'Unchecked_Access, 0) /= 0 then
            return Interfaces.C.To_Ada(Buffer(1 .. Interfaces.C.size_t(Chars_Read) + 1));
        else
            return "";
        end if;
    end Get_Input;

    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    function Get_Line(Format_Fn     : Format_Function := null;
                      Completion_Fn : Completion_Function := null) return String
    is
        package TTI renames Trendy_Terminal.Input;
        use all type Interfaces.C.int;

        Input_Line : ASU.Unbounded_String;
        Input      : Interfaces.C.int;
        Key_Enter  : constant := 13;
        KM         : constant Trendy_Terminal.Maps.Key_Maps.Map := Trendy_Terminal.Maps.Make_Key_Map;
        MK         : constant Trendy_Terminal.Maps.Inverse_Key_Maps.Map := Trendy_Terminal.Maps.Make_Key_Lookup_Map;
        L          : Trendy_Terminal.Input.Line_Input;
        Line_Pos   : constant Cursor_Position := Trendy_Terminal.VT100.Get_Cursor_Position;
        Edit_Pos   : Cursor_Position := Line_Pos;

        -- Prints an updated input line at the given starting position.
        procedure Print_Line (Pos : Cursor_Position; S : String) is
        begin
            VT100.Set_Cursor_Position (Pos);
            VT100.Clear_Line;
            Put (S);
        end Print_Line;

        use Trendy_Terminal.Maps;
    begin
        loop
            -- Clear anything which has been printed and then print the current
            -- state of the line.

            if Format_Fn /= null then
                Print_Line (Line_Pos, Format_Fn (TTI.Current (L)));
            else
                Print_Line (Line_Pos, TTI.Current (L));
            end if;

            Edit_Pos.Row := Line_Pos.Row;
            Edit_Pos.Col := TTI.Cursor_Index(L);
            VT100.Set_Cursor_Position (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Get_Input);

            if MK(Key_Left) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Left);
            elsif MK(Key_Right) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Right);
            elsif MK(Key_Backspace) = Input_Line then
                TTI.Backspace (L);
            elsif MK(Key_Delete) = Input_Line then
                TTI.Delete (L);
            elsif MK(Key_Tab) = Input_Line then
                -- Do tab completion on the line
                if Completion_Fn /= null then
                    -- Adjust the cursor position?
                    null;
                end if;
            elsif ASU.Length (Input_Line) = 1 then
                Input := Character'Pos(ASU.Element(Input_Line, 1));

                -- Line has been finished.
                if Input = Key_Enter then
                    return TTI.Current(L);
                end if;
            end if;

            -- Actual text was inserted.
            -- TODO: Maybe add a "replace" mode?
            if not KM.Contains (Input_Line) then
                TTI.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Get_Line;

    -- Processes the next line of input in according to completion, formatting,
    -- and hinting callbacks.
    --
    -- TODO: Support full utf-8.  Only ASCII is supported for now.
    function Debug_Get_Line (Format_Fn     : Format_Function := null;
                             Completion_Fn : Completion_Function := null;
                             Debug_Fn      : Format_Function := null) return String
    is
        package TTI renames Trendy_Terminal.Input;
        use all type Interfaces.C.int;

        Input_Line : ASU.Unbounded_String;
        Input      : Interfaces.C.int;
        Key_Enter  : constant := 13;
        KM         : constant Trendy_Terminal.Maps.Key_Maps.Map := Trendy_Terminal.Maps.Make_Key_Map;
        MK         : constant Trendy_Terminal.Maps.Inverse_Key_Maps.Map := Trendy_Terminal.Maps.Make_Key_Lookup_Map;
        L          : Trendy_Terminal.Input.Line_Input;
        Line_Pos   : constant Cursor_Position := VT100.Get_Cursor_Position;
        Debug_Pos  : Cursor_Position := Line_Pos;
        Edit_Pos   : Cursor_Position := Line_Pos;

        function Format_Line (S : String) return String is
            use Ada.Characters.Latin_1;
        begin
            return Quotation & S & Quotation;
        end Format_Line;

        -- Prints an updated input line at the given starting position.
        procedure Print_Line (Pos : Cursor_Position; S : String) is
        begin
            VT100.Set_Cursor_Position (Pos);
            VT100.Clear_Line;
            Put (S);
        end Print_Line;

        use Trendy_Terminal.Maps;
    begin
        pragma Unreferenced (Format_Fn, Completion_Fn);

        Debug_Pos.Row := Debug_Pos.Row - 1;

        loop
            if Debug_Fn /= null then
                Print_Line (Debug_Pos, Debug_Fn (TTI.Current (L)));
            else
                Print_Line (Debug_Pos, "Cursor @ " & TTI.Cursor_Index(L)'Image);
            end if;

            -- Clear anything which has been printed and then print the current
            -- state of the line.
            Print_Line (Line_Pos, Format_Line (TTI.Current (L)));

            Edit_Pos.Row := Line_Pos.Row;
            Edit_Pos.Col := TTI.Cursor_Index(L) + 1; -- add 1 for the " around the debug line
            VT100.Set_Cursor_Position (Edit_Pos);

            -- Get and process the new input.
            Input_Line := ASU.To_Unbounded_String(Get_Input);

            if MK(Key_Left) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Left);
            elsif MK(Key_Right) = Input_Line then
                TTI.Move_Cursor(L, Trendy_Terminal.Input.Right);
            elsif MK(Key_Backspace) = Input_Line then
                TTI.Backspace (L);
            elsif MK(Key_Delete) = Input_Line then
                TTI.Delete (L);
            elsif ASU.Length (Input_Line) = 1 then
                Input := Character'Pos(ASU.Element(Input_Line, 1));

                -- Line has been finished.
                if Input = Key_Enter then
                    return TTI.Current(L);
                end if;
            end if;

            -- Actual text was inserted.
            -- TODO: Maybe add a "replace" mode?
            if not KM.Contains (Input_Line) then
                TTI.Insert (L, ASU.To_String (Input_Line));
            end if;
        end loop;
    end Debug_Get_Line;
end Trendy_Terminal;
