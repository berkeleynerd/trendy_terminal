-------------------------------------------------------------------------------
-- Copyright 2021, The Trendy Terminal Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Interfaces.C.Strings;

with System;

with Trendy_Terminal.Mac;
with Trendy_Terminal.Maps;

package body Trendy_Terminal.Platform is
    use type Mac.BOOL;

    ---------------------------------------------------------------------------
    --
    ---------------------------------------------------------------------------
    type IOStream is record
        File            : Mac.FILE_Ptr;
        File_Descriptor : Mac.FD;
        Settings        : aliased Mac.Termios;
    end record;

    ---------------------------------------------------------------------------
    -- Original settings
    ---------------------------------------------------------------------------
    Original_Input_Setting, Original_Output_Setting, Original_Error_Setting : Mac.Termios;

    ---------------------------------------------------------------------------
    -- The triad of I/O streams.
    ---------------------------------------------------------------------------
    Std_Input, Std_Output, Std_Error : IOStream;

    function Load_Terminal (File_Descriptor : Mac.FD; Terminal : not null access Mac.Termios) return Boolean is
    begin
        if Mac.isatty (File_Descriptor) = 0 then
            Ada.Text_IO.Put_Line ("Unable to load file descriptor for terminal.");
            return False;
        end if;
        if Mac.tcgetattr (File_Descriptor, Terminal.all'Address) /= 0 then
            Ada.Text_IO.Put_Line ("Unable to get terminal attributes.");
            return False;
        end if;
        return True;
    end Load_Terminal;

    function Make_Handle (File_Descriptor : Mac.FILE_Ptr; Handle : out IOStream) return Boolean is
    begin
        Handle.File            := File_Descriptor;
        Handle.File_Descriptor := Mac.fileno (Handle.File);
        return Load_Terminal (Handle.File_Descriptor, Handle.Settings'Unchecked_Access);
    end Make_Handle;

    procedure Require_Settings_Change (Stream : IOStream) with
        Pre => Mac.isatty (Stream.File_Descriptor) /= 0
    is
    begin
        if Mac.tcsetattr (Stream.File_Descriptor, Mac.TCSANOW, Stream.Settings'Address) /= 0 then
            Ada.Text_IO.Put_Line ("Unable to change settings.");
        end if;
    end Require_Settings_Change;

    function Init return Boolean is
        -- Initializes and captures the original settings for the terminal so they can
        -- be restored when the system is shutdown.
    begin
        if not Make_Handle (Mac.stdin, Std_Input) or else not Make_Handle (Mac.stdout, Std_Output)
            or else not Make_Handle (Mac.stderr, Std_Error)
        then
            Ada.Text_IO.Put_Line ("Unable to get standard stream handles.");
        end if;

        -- Save the startup settings.
        Original_Input_Setting  := Std_Input.Settings;
        Original_Output_Setting := Std_Output.Settings;
        Original_Error_Setting  := Std_Error.Settings;
        return True;
    end Init;

    procedure Shutdown is
    begin
        Std_Input.Settings  := Original_Input_Setting;
        Std_Output.Settings := Original_Output_Setting;
        Std_Error.Settings  := Original_Error_Setting;

        Require_Settings_Change (Std_Input);
        Require_Settings_Change (Std_Output);
        Require_Settings_Change (Std_Error);
    end Shutdown;

    procedure Set (Setting : Platform.Input_Setting; Enabled : Boolean) is
    begin
        case Setting is
            when Platform.IGNBRK =>
                Std_Input.Settings.c_iflag (Mac.IGNBRK) := Enabled;
            when Platform.BRKINT =>
                Std_Input.Settings.c_iflag (Mac.BRKINT) := Enabled;
            when Platform.IGNPAR =>
                Std_Input.Settings.c_iflag (Mac.IGNPAR) := Enabled;
            when Platform.PARMRK =>
                Std_Input.Settings.c_iflag (Mac.PARMRK) := Enabled;
            when Platform.INPCK =>
                Std_Input.Settings.c_iflag (Mac.INPCK) := Enabled;
            when Platform.ISTRIP =>
                Std_Input.Settings.c_iflag (Mac.ISTRIP) := Enabled;
            when Platform.INLCR =>
                Std_Input.Settings.c_iflag (Mac.INLCR) := Enabled;
            when Platform.IGNCR =>
                Std_Input.Settings.c_iflag (Mac.IGNCR) := Enabled;
            when Platform.ICRNL =>
                Std_Input.Settings.c_iflag (Mac.ICRNL) := Enabled;
            when Platform.IXON =>
                Std_Input.Settings.c_iflag (Mac.IXON) := Enabled;
            when Platform.IXOFF =>
                Std_Input.Settings.c_iflag (Mac.IXOFF) := Enabled;
            when Platform.IXANY =>
                Std_Input.Settings.c_iflag (Mac.IXANY) := Enabled;
            when Platform.OPOST =>
                Std_Input.Settings.c_oflag (Mac.OPOST) := Enabled;
            when Platform.ONLCR =>
                Std_Input.Settings.c_oflag (Mac.ONLCR) := Enabled;
            when Platform.OXTABS =>
                Std_Input.Settings.c_oflag (Mac.OXTABS) := Enabled;
            when Platform.ONOEOT =>
                Std_Input.Settings.c_oflag (Mac.ONOEOT) := Enabled;
            when Platform.ECHOKE =>
                Std_Input.Settings.c_lflag (Mac.ECHOKE) := Enabled;
            when Platform.ECHOE =>
                Std_Input.Settings.c_lflag (Mac.ECHOE) := Enabled;
            when Platform.ECHOK =>
                Std_Input.Settings.c_lflag (Mac.ECHOK) := Enabled;
            when Platform.ECHO =>
                Std_Input.Settings.c_lflag (Mac.ECHO) := Enabled;
            when Platform.ECHONL =>
                Std_Input.Settings.c_lflag (Mac.ECHONL) := Enabled;
            when Platform.ECHOPRT =>
                Std_Input.Settings.c_lflag (Mac.ECHOPRT) := Enabled;
            when Platform.ECHOCTL =>
                Std_Input.Settings.c_lflag (Mac.ECHOCTL) := Enabled;
            when Platform.ISIG =>
                Std_Input.Settings.c_lflag (Mac.ISIG) := Enabled;
            when Platform.ICANON =>
                Std_Input.Settings.c_lflag (Mac.ICANON) := Enabled;
            when Platform.ALTWERASE =>
                Std_Input.Settings.c_lflag (Mac.ALTWERASE) := Enabled;
            when Platform.IEXTEN =>
                Std_Input.Settings.c_lflag (Mac.IEXTEN) := Enabled;
            when Platform.EXTPROC =>
                Std_Input.Settings.c_lflag (Mac.EXTPROC) := Enabled;
            when Platform.TOSTOP =>
                Std_Input.Settings.c_lflag (Mac.TOSTOP) := Enabled;
            when Platform.FLUSHO =>
                Std_Input.Settings.c_lflag (Mac.FLUSHO) := Enabled;
            when Platform.NOKERNINFO =>
                Std_Input.Settings.c_lflag (Mac.NOKERNINFO) := Enabled;
            when Platform.PENDIN =>
                Std_Input.Settings.c_lflag (Mac.PENDIN) := Enabled;
            when Platform.NOFLSH =>
                Std_Input.Settings.c_lflag (Mac.NOFLSH) := Enabled;
            when Platform.CS5 =>
                Std_Input.Settings.c_cflag (Mac.CS5) := Enabled;
            when Platform.CIGNORE =>
                Std_Input.Settings.c_cflag (Mac.CIGNORE) := Enabled;
            when Platform.CS6 =>
                Std_Input.Settings.c_cflag (Mac.CS6) := Enabled;
            when Platform.CS7 =>
                Std_Input.Settings.c_cflag (Mac.CS7) := Enabled;
            when Platform.CS8 =>
                Std_Input.Settings.c_cflag (Mac.CS8) := Enabled;
            when Platform.CSTOPB =>
                Std_Input.Settings.c_cflag (Mac.CSTOPB) := Enabled;
            when Platform.CREAD =>
                Std_Input.Settings.c_cflag (Mac.CREAD) := Enabled;
            when Platform.PARENB =>
                Std_Input.Settings.c_cflag (Mac.PARENB) := Enabled;
            when Platform.PARODD =>
                Std_Input.Settings.c_cflag (Mac.PARODD) := Enabled;
            when Platform.HUPCL =>
                Std_Input.Settings.c_cflag (Mac.HUPCL) := Enabled;
            when Platform.CLOCAL =>
                Std_Input.Settings.c_cflag (Mac.CLOCAL) := Enabled;
            when Platform.CCTS_OFLOW =>
                Std_Input.Settings.c_cflag (Mac.CCTS_OFLOW) := Enabled;
            when Platform.CRTS_IFLOW =>
                Std_Input.Settings.c_cflag (Mac.CRTS_IFLOW) := Enabled;
            when Platform.CDTR_IFLOW =>
                Std_Input.Settings.c_cflag (Mac.CDTR_IFLOW) := Enabled;
            when Platform.CDSR_OFLOW =>
                Std_Input.Settings.c_cflag (Mac.CDSR_OFLOW) := Enabled;
            when Platform.CCAR_OFLOW =>
                Std_Input.Settings.c_cflag (Mac.CCAR_OFLOW) := Enabled;
            when others =>
                raise Ada.Text_IO.Use_Error;
        end case;
        Require_Settings_Change (Std_Input);
    end Set;

    procedure Set (Setting : Output_Setting; Enabled : Boolean) is
    begin
        pragma Unreferenced (Enabled);
        case Setting is
            when Escape_Sequences =>
                null;
                -- nothing to do here
        end case;
    end Set;

    procedure Put (C : Character) renames Ada.Text_IO.Put;
    procedure Put (S : String) renames Ada.Text_IO.Put;

    type VOIDP is new Interfaces.C.Strings.chars_ptr;
    function Read (File_Descriptor : Mac.FD; Buffer : VOIDP; Buffer_Size : Natural) return Integer with
        Import => True, Convention => C;

    -- Gets an entire input line from one keypress.  E.g. all the characters
    -- received for a controlling keypress, such as an arrow key.
    function Get_Input return String is
        Buffer_Size : constant                        := 512;
        Buffer      : aliased Interfaces.C.char_array := (1 .. Interfaces.C.size_t (Buffer_Size) => Interfaces.C.nul);
        Chars_Read  : Integer;
        use all type Interfaces.C.size_t;
    begin
        Chars_Read :=
            Read
                (Mac.fileno (Std_Input.File), VOIDP (Interfaces.C.Strings.To_Chars_Ptr (Buffer'Unchecked_Access)),
                 Buffer_Size);
        if Chars_Read > 0 then
            return Interfaces.C.To_Ada (Buffer (1 .. Interfaces.C.size_t (Chars_Read) + 1));
        else
            return "";
        end if;
    end Get_Input;

    function End_Of_Line return String is
    begin
        return (1 => Maps.Characters.LF);
    end End_Of_Line;

    procedure Print_Configuration is
        function To_Integer1 is new Ada.Unchecked_Conversion (Mac.c_lflag_t, Interfaces.C.long);
        function To_Integer2 is new Ada.Unchecked_Conversion (Mac.c_iflag_t, Interfaces.C.long);
        function To_Integer3 is new Ada.Unchecked_Conversion (Mac.c_oflag_t, Interfaces.C.long);
        function To_Integer4 is new Ada.Unchecked_Conversion (Mac.c_cflag_t, Interfaces.C.long);
    begin
        Ada.Text_IO.Put_Line ("Original Local Settings : " & To_Integer1 (Original_Input_Setting.c_lflag)'Image);
        Ada.Text_IO.Put_Line ("Local Input Mode          : " & To_Integer1 (Std_Input.Settings.c_lflag)'Image);

        Ada.Text_IO.Put_Line ("");

        Ada.Text_IO.Put_Line ("Original Input Settings   : " & To_Integer2 (Original_Input_Setting.c_iflag)'Image);
        Ada.Text_IO.Put_Line ("Input Settings            : " & To_Integer2 (Std_Input.Settings.c_iflag)'Image);

        Ada.Text_IO.Put_Line ("");

        Ada.Text_IO.Put_Line ("Original Output Settings   : " & To_Integer3 (Original_Input_Setting.c_oflag)'Image);
        Ada.Text_IO.Put_Line ("Output Settings            : " & To_Integer3 (Std_Input.Settings.c_oflag)'Image);

        Ada.Text_IO.Put_Line ("");

        Ada.Text_IO.Put_Line ("Original Control Settings  : " & To_Integer4 (Original_Input_Setting.c_cflag)'Image);
        Ada.Text_IO.Put_Line ("Output Settings            : " & To_Integer4 (Std_Input.Settings.c_cflag)'Image);

        Ada.Text_IO.Put_Line ("");

        Ada.Text_IO.Put_Line ("Original Output Mode      : " & To_Integer1 (Original_Output_Setting.c_lflag)'Image);
        Ada.Text_IO.Put_Line ("Output Mode               : " & To_Integer1 (Std_Output.Settings.c_lflag)'Image);

        Ada.Text_IO.Put_Line ("");

        Ada.Text_IO.Put_Line ("Original Error Mode       : " & To_Integer1 (Original_Error_Setting.c_lflag)'Image);
        Ada.Text_IO.Put_Line ("Error Mode                : " & To_Integer1 (Std_Error.Settings.c_lflag)'Image);
    end Print_Configuration;

end Trendy_Terminal.Platform;
