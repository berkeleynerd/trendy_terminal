with Ada.Strings.Unbounded;

-- Trendy Terminal defines a known environment in which to perform input/output.
-- Failure to meet these requirements results in an failed initialization.
--
-- The requirements:
-- 1. UTF-8
-- 2. VT100 terminal escape sequences.
--
-- The base package provides platform-specific environment setup, and the basic
-- read/write commands on top of which to build functionality.
package Trendy_Terminal is
    package ASU renames Ada.Strings.Unbounded;

    -- Initializes and captures the original settings for the terminal so they can
    -- be restored when the system is shutdown.
    function Init return Boolean;

    -- Restores the system to the conditions prior to calling `Init`.
    procedure Shutdown;

    type Input_Setting is (Echo, Line_Input);
    type Output_Setting is (Escape_Sequences);

    procedure Set (Setting : Input_Setting; Enabled : Boolean);
    procedure Set (Setting : Output_Setting; Enabled : Boolean);

    -- These are platform-specific terminal read/write functions to avoid
    -- messing with Ada standard library internals such as current column.
    -- This avoids spurious formatting and other implementation quirks of
    -- those libraries.
    procedure Put (C : Character);
    procedure Put (S : String);
    procedure Put_Line (S : String);

    type Format_Function is access function (S : String) return String;
    type Completion_Function is access function (S: String) return String;

    function Get_Input return String;
    function Get_Line (Format_Fn     : Format_Function := null;
                       Completion_Fn : Completion_Function := null) return String;

    -- A debug version of Get_Line for learning how to write an appropriate
    -- interface and callback system for Get_Line.
    function Debug_Get_Line (Format_Fn     : Format_Function := null;
                             Completion_Fn : Completion_Function := null;
                             Debug_Fn      : Format_Function := null) return String;

    type Cursor_Position is record
        Row : Integer;
        Col : Integer;
    end record;

    function Get_Cursor_Position return Cursor_Position;

    type Key is (Key_Up, Key_Left, Key_Right, Key_Down,
                  Key_Ctrl_Up, Key_Ctrl_Left, Key_Ctrl_Right, Key_Ctrl_Down,
                  Key_F1, Key_F2, Key_F3, Key_F4,
                  Key_F5, Key_F6, Key_F7, Key_F8,
                  Key_F9, Key_F10, Key_F11, Key_F12,
                  Key_Backspace, Key_Pause, Key_Escape,
                  Key_Home, Key_End,
                  Key_Insert, Key_Delete,
                  Key_Page_Up, Key_Page_Down,
                  Key_Tab);
end Trendy_Terminal;
