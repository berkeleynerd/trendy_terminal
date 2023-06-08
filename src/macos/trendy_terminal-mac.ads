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

with Interfaces.C.Strings;
with System;

package Trendy_Terminal.Mac is

    ---------------------------------------------------------------------------
    -- Interfacing with C
    ---------------------------------------------------------------------------
    -- Crash course in how this works.
    -- https://en.wikibooks.org/wiki/Ada_Programming/Types/access#Access_vs._System.Address
    type BOOL is new Interfaces.C.int;
    type FD is new Interfaces.C.int;
    type FILE_Ptr is new System.Address;

    function fileno (Stream : FILE_Ptr) return FD with
        Import => True, Convention => C;

    function isatty (File_Descriptor : FD) return BOOL with
        Import => True, Convention => C;

    stdin  : aliased FILE_Ptr;
    stdout : aliased FILE_Ptr;
    stderr : aliased FILE_Ptr;

    pragma Import (C, stdin, "__stdinp");
    pragma Import (C, stdout, "__stdoutp");
    pragma Import (C, stderr, "__stderrp");

    NCCS : constant := 20;

    type tcflag_t is new Interfaces.C.unsigned_long;
    type cc_t is new Interfaces.C.unsigned_char;
    type speed_t is new Interfaces.C.long;
    type cc_array is array (Natural range 0 .. NCCS - 1) of cc_t with
        Convention => C;

    --!pp off

    --     IGNBRK    ,  -- Unused
    --     BRKINT    ,
    --     IGNPAR    ,  -- Unused
    --     PARMRK    ,  -- Unused
    --     INPCK     ,
    --     ISTRIP    ,
    --     INLCR     ,  -- Unused
    --     IGNCR     ,  -- Unused
    --     ICRNL     ,
    --     Unused6   ,  -- Unused
    --     IXON      ,
    --     Unused7       -- Unused

    type c_iflag_bits is (
        IGNBRK          ,       -- ignore BREAK condition
        BRKINT          ,       -- map BREAK to SIGINTR
        IGNPAR          ,       -- ignore (discard) parity errors
        PARMRK          ,       -- mark parity and framing errors
        INPCK           ,       -- enable checking of parity errors
        ISTRIP          ,       -- strip 8th bit off chars
        INLCR           ,       -- map NL into CR
        IGNCR           ,       -- ignore CR
        ICRNL           ,       -- map CR to NL (ala CRMOD)
        IXON            ,       -- enable output flow control
        IXOFF           ,       -- enable input flow control
        IXANY                   -- any char will restart after stop
       );

    --  for c_iflag_bits use
    --    (
    --     Unused1    => 16#00000001#,
    --     BRKINT     => 16#00000002#,
    --     Unused2    => 16#00000004#,
    --     Unused3    => 16#00000010#,
    --     INPCK      => 16#00000020#,
    --     ISTRIP     => 16#00000040#,
    --     Unused4    => 16#00000100#,
    --     Unused5    => 16#00000200#,
    --     ICRNL      => 16#00000400#,
    --     Unused6    => 16#00001000#,
    --     IXON       => 16#00002000#,
    --     Unused7    => 16#00004000#
    --    );

    for c_iflag_bits use
      (
        IGNBRK         => 16#00000001#,        -- ignore BREAK condition
        BRKINT         => 16#00000002#,        -- map BREAK to SIGINTR
        IGNPAR         => 16#00000004#,        -- ignore (discard) parity errors
        PARMRK         => 16#00000008#,        -- mark parity and framing errors
        INPCK          => 16#00000010#,        -- enable checking of parity errors
        ISTRIP         => 16#00000020#,        -- strip 8th bit off chars
        INLCR          => 16#00000040#,        -- map NL into CR
        IGNCR          => 16#00000080#,        -- ignore CR
        ICRNL          => 16#00000100#,        -- map CR to NL (ala CRMOD)
        IXON           => 16#00000200#,        -- enable output flow control
        IXOFF          => 16#00000400#,        -- enable input flow control
        IXANY          => 16#00000800#         -- any char will restart after stop
      );

    
    --  type c_lflag_bits is (
    --     ECHOKE     ,
    --     ECHOE      ,
    --     ECHOK      ,
    --     ECHO       ,
    --     ECHONL     ,
    --     ECHOPRT    ,
    --     ECHOCTL    ,
    --     ISIG       ,
    --     ICANON     ,
    --     ALTWERASE  ,
    --     IEXTEN     ,
    --     EXTPROC    ,
    --     Unused1    ,
    --     Unused2    ,
    --     TOSTOP     ,
    --     FLUSHO     ,
    --     Unused3    ,
    --     NOKERNINFO ,
    --     Unused4    ,
    --     Unused5    ,
    --     Unused6    ,
    --     PENDIN     ,
    --     NOFLSH);

    type c_lflag_bits is (
        ECHOKE      ,  -- visual erase for line kill
        ECHOE       ,  -- visually erase chars
        ECHOK       ,  -- echo NL after line kill
        ECHO        ,  -- enable echoing
        ECHONL      ,  -- echo NL even if ECHO is off
        ECHOPRT     ,  -- visual erase mode for hardcopy
        ECHOCTL     ,  -- echo control chars as ^(Char)
        ISIG        ,  -- enable signals INTR, QUIT, [D]SUSP
        ICANON      ,  -- canonicalize input lines
        ALTWERASE   ,  -- use alternate WERASE algorithm
        IEXTEN      ,  -- enable DISCARD and LNEXT
        EXTPROC     ,  -- external processing
        TOSTOP      ,  -- stop background jobs from output
        FLUSHO      ,  -- output being flushed (state)
        NOKERNINFO  ,  -- no kernel output from VSTATUS
        PENDIN      ,  -- XXX retype pending input (state)
        NOFLSH         -- don't flush after interrupt
    );

    --  for c_lflag_bits use
    --    (
    --     ECHOKE     => 16#00000001#,
    --     ECHOE      => 16#00000002#,
    --     ECHOK      => 16#00000004#,
    --     ECHO       => 16#00000008#,
    --     ECHONL     => 16#00000010#,
    --     ECHOPRT    => 16#00000020#,
    --     ECHOCTL    => 16#00000040#,
    --     ISIG       => 16#00000080#,
    --     ICANON     => 16#00000100#,
    --     ALTWERASE  => 16#00000200#,
    --     IEXTEN     => 16#00000400#,
    --     EXTPROC    => 16#00000800#,
    --     Unused1    => 16#00100000#,
    --     Unused2    => 16#00200000#,
    --     TOSTOP     => 16#00400000#,
    --     FLUSHO     => 16#00800000#,
    --     Unused3    => 16#01000000#,
    --     NOKERNINFO => 16#02000000#,
    --     Unused4    => 16#04000000#,
    --     Unused5    => 16#08000000#,
    --     Unused6    => 16#10000000#,
    --     PENDIN     => 16#20000000#,
    --     NOFLSH     => 16#80000000#
    --    );

    for c_lflag_bits use
      (
        ECHOKE      => 16#00000001#,  -- visual erase for line kill
        ECHOE       => 16#00000002#,  -- visually erase chars
        ECHOK       => 16#00000004#,  -- echo NL after line kill
        ECHO        => 16#00000008#,  -- enable echoing
        ECHONL      => 16#00000010#,  -- echo NL even if ECHO is off
        ECHOPRT     => 16#00000020#,  -- visual erase mode for hardcopy
        ECHOCTL     => 16#00000040#,  -- echo control chars as ^(Char)
        ISIG        => 16#00000080#,  -- enable signals INTR, QUIT, [D]SUSP
        ICANON      => 16#00000100#,  -- canonicalize input lines
        ALTWERASE   => 16#00000200#,  -- use alternate WERASE algorithm
        IEXTEN      => 16#00000400#,  -- enable DISCARD and LNEXT
        EXTPROC     => 16#00000800#,  -- external processing
        TOSTOP      => 16#00400000#,  -- stop background jobs from output
        FLUSHO      => 16#00800000#,  -- output being flushed (state)
        NOKERNINFO  => 16#02000000#,  -- no kernel output from VSTATUS
        PENDIN      => 16#20000000#,  -- XXX retype pending input (state)
        NOFLSH      => 16#80000000#   -- don't flush after interrupt
      );

    --!pp on

    pragma Warnings (Off, "bits of *unused");
    type c_lflag_t is array (c_lflag_bits) of Boolean with
        Pack, Size => tcflag_t'Size;
    type c_iflag_t is array (c_iflag_bits) of Boolean with
        Pack, Size => tcflag_t'Size;
    pragma Warnings (On, "bits of *unused");

    type Termios is record
        c_iflag  : c_iflag_t;
        c_oflag  : tcflag_t;
        c_cflag  : tcflag_t;
        c_lflag  : c_lflag_t;
        c_line   : cc_t;
        c_cc     : cc_array;
        c_ispeed : speed_t;
        c_ospeed : speed_t;
    end record with
        Convention => C;

    function tcgetattr (File_Descriptor : FD; Terminal : System.Address) return BOOL with
        Import => True, Convention => C;

    type Application_Time is
        (TCSANOW,   -- immediate effect
         TCSADRAIN, -- after all output written
         TCSAFLUSH  -- like drain, except input received as well
    );
    for Application_Time use (TCSANOW => 0, TCSADRAIN => 1, TCSAFLUSH => 2);

    function tcsetattr
        (File_Descriptor : FD; Effect_Time : Application_Time; Terminal : System.Address) return BOOL with
        Import => True, Convention => C;

end Trendy_Terminal.Mac;
