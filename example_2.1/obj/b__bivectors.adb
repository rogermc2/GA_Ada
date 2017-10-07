pragma Ada_95;
pragma Warnings (Off);
pragma Source_File_Name (ada_main, Spec_File_Name => "b__bivectors.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__bivectors.adb");
pragma Suppress (Overflow_Check);
with Ada.Exceptions;

package body ada_main is

   E077 : Short_Integer; pragma Import (Ada, E077, "system__os_lib_E");
   E011 : Short_Integer; pragma Import (Ada, E011, "system__soft_links_E");
   E021 : Short_Integer; pragma Import (Ada, E021, "system__exception_table_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "ada__containers_E");
   E065 : Short_Integer; pragma Import (Ada, E065, "ada__io_exceptions_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "ada__numerics_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "ada__strings_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "ada__strings__maps_E");
   E366 : Short_Integer; pragma Import (Ada, E366, "ada__strings__maps__constants_E");
   E049 : Short_Integer; pragma Import (Ada, E049, "ada__tags_E");
   E064 : Short_Integer; pragma Import (Ada, E064, "ada__streams_E");
   E075 : Short_Integer; pragma Import (Ada, E075, "interfaces__c_E");
   E088 : Short_Integer; pragma Import (Ada, E088, "interfaces__c__strings_E");
   E023 : Short_Integer; pragma Import (Ada, E023, "system__exceptions_E");
   E080 : Short_Integer; pragma Import (Ada, E080, "system__file_control_block_E");
   E179 : Short_Integer; pragma Import (Ada, E179, "ada__streams__stream_io_E");
   E070 : Short_Integer; pragma Import (Ada, E070, "system__file_io_E");
   E073 : Short_Integer; pragma Import (Ada, E073, "system__finalization_root_E");
   E071 : Short_Integer; pragma Import (Ada, E071, "ada__finalization_E");
   E165 : Short_Integer; pragma Import (Ada, E165, "system__storage_pools_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "system__finalization_masters_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__storage_pools__subpools_E");
   E353 : Short_Integer; pragma Import (Ada, E353, "ada__calendar_E");
   E357 : Short_Integer; pragma Import (Ada, E357, "ada__calendar__time_zones_E");
   E375 : Short_Integer; pragma Import (Ada, E375, "system__direct_io_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "system__pool_global_E");
   E015 : Short_Integer; pragma Import (Ada, E015, "system__secondary_stack_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "ada__strings__unbounded_E");
   E351 : Short_Integer; pragma Import (Ada, E351, "ada__directories_E");
   E371 : Short_Integer; pragma Import (Ada, E371, "system__regexp_E");
   E062 : Short_Integer; pragma Import (Ada, E062, "ada__text_io_E");
   E102 : Short_Integer; pragma Import (Ada, E102, "gl_E");
   E112 : Short_Integer; pragma Import (Ada, E112, "gl__types_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "gl__context_E");
   E133 : Short_Integer; pragma Import (Ada, E133, "gl__errors_E");
   E147 : Short_Integer; pragma Import (Ada, E147, "gl__objects_E");
   E149 : Short_Integer; pragma Import (Ada, E149, "gl__objects__buffers_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "gl__objects__shaders_E");
   E195 : Short_Integer; pragma Import (Ada, E195, "gl__objects__shaders__lists_E");
   E185 : Short_Integer; pragma Import (Ada, E185, "gl__objects__renderbuffers_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "gl__types__colors_E");
   E137 : Short_Integer; pragma Import (Ada, E137, "gl__fixed__lighting_E");
   E296 : Short_Integer; pragma Import (Ada, E296, "gl__immediate_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "gl__objects__textures_E");
   E304 : Short_Integer; pragma Import (Ada, E304, "ft_E");
   E309 : Short_Integer; pragma Import (Ada, E309, "ft__faces_E");
   E313 : Short_Integer; pragma Import (Ada, E313, "ft__glyphs_E");
   E183 : Short_Integer; pragma Import (Ada, E183, "gl__objects__framebuffers_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "gl__objects__programs_E");
   E316 : Short_Integer; pragma Import (Ada, E316, "gl__objects__textures__targets_E");
   E311 : Short_Integer; pragma Import (Ada, E311, "ft__ogl_E");
   E278 : Short_Integer; pragma Import (Ada, E278, "ga_maths_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "e2ga_E");
   E282 : Short_Integer; pragma Import (Ada, E282, "e2ga_draw_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "e3ga_E");
   E290 : Short_Integer; pragma Import (Ada, E290, "e3ga_utilities_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "ga_draw_E");
   E294 : Short_Integer; pragma Import (Ada, E294, "geosphere_E");
   E373 : Short_Integer; pragma Import (Ada, E373, "gl__files_E");
   E298 : Short_Integer; pragma Import (Ada, E298, "gl__objects__vertex_arrays_E");
   E347 : Short_Integer; pragma Import (Ada, E347, "gl__raster_E");
   E302 : Short_Integer; pragma Import (Ada, E302, "gl__window_E");
   E300 : Short_Integer; pragma Import (Ada, E300, "gl_util_E");
   E082 : Short_Integer; pragma Import (Ada, E082, "glfw_E");
   E086 : Short_Integer; pragma Import (Ada, E086, "glfw__errors_E");
   E090 : Short_Integer; pragma Import (Ada, E090, "glfw__input_E");
   E092 : Short_Integer; pragma Import (Ada, E092, "glfw__input__joysticks_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "glfw__monitors_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "glfw__windows_E");
   E208 : Short_Integer; pragma Import (Ada, E208, "glfw__windows__context_E");
   E083 : Short_Integer; pragma Import (Ada, E083, "glfw__api_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "glfw__windows__hints_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "initialize_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "main_loop_E");
   E249 : Short_Integer; pragma Import (Ada, E249, "maths_E");
   E276 : Short_Integer; pragma Import (Ada, E276, "multivector_type_base_E");
   E274 : Short_Integer; pragma Import (Ada, E274, "multivector_analysis_E");
   E349 : Short_Integer; pragma Import (Ada, E349, "program_loader_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "quaternions_E");
   E377 : Short_Integer; pragma Import (Ada, E377, "silo_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "utilities_E");

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure finalize_library is
   begin
      declare
         procedure F1;
         pragma Import (Ada, F1, "ga_draw__finalize_body");
      begin
         E284 := E284 - 1;
         F1;
      end;
      declare
         procedure F2;
         pragma Import (Ada, F2, "silo__finalize_body");
      begin
         E377 := E377 - 1;
         F2;
      end;
      E092 := E092 - 1;
      E098 := E098 - 1;
      E100 := E100 - 1;
      declare
         procedure F3;
         pragma Import (Ada, F3, "glfw__windows__finalize_spec");
      begin
         F3;
      end;
      declare
         procedure F4;
         pragma Import (Ada, F4, "glfw__monitors__finalize_spec");
      begin
         F4;
      end;
      declare
         procedure F5;
         pragma Import (Ada, F5, "glfw__input__joysticks__finalize_spec");
      begin
         F5;
      end;
      declare
         procedure F6;
         pragma Import (Ada, F6, "ft__ogl__finalize_body");
      begin
         E311 := E311 - 1;
         F6;
      end;
      declare
         procedure F7;
         pragma Import (Ada, F7, "gl__objects__vertex_arrays__finalize_body");
      begin
         E298 := E298 - 1;
         F7;
      end;
      declare
         procedure F8;
         pragma Import (Ada, F8, "gl__objects__vertex_arrays__finalize_spec");
      begin
         F8;
      end;
      E294 := E294 - 1;
      declare
         procedure F9;
         pragma Import (Ada, F9, "geosphere__finalize_spec");
      begin
         F9;
      end;
      E316 := E316 - 1;
      declare
         procedure F10;
         pragma Import (Ada, F10, "gl__objects__textures__targets__finalize_spec");
      begin
         F10;
      end;
      declare
         procedure F11;
         pragma Import (Ada, F11, "gl__objects__buffers__finalize_body");
      begin
         E149 := E149 - 1;
         F11;
      end;
      E193 := E193 - 1;
      declare
         procedure F12;
         pragma Import (Ada, F12, "gl__objects__renderbuffers__finalize_body");
      begin
         E185 := E185 - 1;
         F12;
      end;
      E137 := E137 - 1;
      E296 := E296 - 1;
      declare
         procedure F13;
         pragma Import (Ada, F13, "gl__objects__textures__finalize_body");
      begin
         E187 := E187 - 1;
         F13;
      end;
      declare
         procedure F14;
         pragma Import (Ada, F14, "gl__objects__framebuffers__finalize_body");
      begin
         E183 := E183 - 1;
         F14;
      end;
      E191 := E191 - 1;
      declare
         procedure F15;
         pragma Import (Ada, F15, "gl__objects__programs__finalize_spec");
      begin
         F15;
      end;
      declare
         procedure F16;
         pragma Import (Ada, F16, "gl__objects__framebuffers__finalize_spec");
      begin
         F16;
      end;
      E313 := E313 - 1;
      declare
         procedure F17;
         pragma Import (Ada, F17, "ft__glyphs__finalize_spec");
      begin
         F17;
      end;
      E304 := E304 - 1;
      E309 := E309 - 1;
      declare
         procedure F18;
         pragma Import (Ada, F18, "ft__faces__finalize_spec");
      begin
         F18;
      end;
      declare
         procedure F19;
         pragma Import (Ada, F19, "ft__finalize_spec");
      begin
         F19;
      end;
      declare
         procedure F20;
         pragma Import (Ada, F20, "gl__objects__textures__finalize_spec");
      begin
         F20;
      end;
      declare
         procedure F21;
         pragma Import (Ada, F21, "gl__immediate__finalize_spec");
      begin
         F21;
      end;
      declare
         procedure F22;
         pragma Import (Ada, F22, "gl__fixed__lighting__finalize_spec");
      begin
         F22;
      end;
      declare
         procedure F23;
         pragma Import (Ada, F23, "gl__objects__renderbuffers__finalize_spec");
      begin
         F23;
      end;
      declare
         procedure F24;
         pragma Import (Ada, F24, "gl__objects__shaders__finalize_spec");
      begin
         F24;
      end;
      declare
         procedure F25;
         pragma Import (Ada, F25, "gl__objects__buffers__finalize_spec");
      begin
         F25;
      end;
      E062 := E062 - 1;
      declare
         procedure F26;
         pragma Import (Ada, F26, "ada__text_io__finalize_spec");
      begin
         F26;
      end;
      E351 := E351 - 1;
      E371 := E371 - 1;
      declare
         procedure F27;
         pragma Import (Ada, F27, "system__regexp__finalize_spec");
      begin
         F27;
      end;
      declare
         procedure F28;
         pragma Import (Ada, F28, "ada__directories__finalize_spec");
      begin
         F28;
      end;
      E217 := E217 - 1;
      declare
         procedure F29;
         pragma Import (Ada, F29, "ada__strings__unbounded__finalize_spec");
      begin
         F29;
      end;
      declare
         procedure F30;
         pragma Import (Ada, F30, "system__file_io__finalize_body");
      begin
         E070 := E070 - 1;
         F30;
      end;
      E159 := E159 - 1;
      E173 := E173 - 1;
      E169 := E169 - 1;
      declare
         procedure F31;
         pragma Import (Ada, F31, "system__pool_global__finalize_spec");
      begin
         F31;
      end;
      E375 := E375 - 1;
      declare
         procedure F32;
         pragma Import (Ada, F32, "system__direct_io__finalize_spec");
      begin
         F32;
      end;
      declare
         procedure F33;
         pragma Import (Ada, F33, "system__storage_pools__subpools__finalize_spec");
      begin
         F33;
      end;
      declare
         procedure F34;
         pragma Import (Ada, F34, "system__finalization_masters__finalize_spec");
      begin
         F34;
      end;
      E179 := E179 - 1;
      declare
         procedure F35;
         pragma Import (Ada, F35, "ada__streams__stream_io__finalize_spec");
      begin
         F35;
      end;
      declare
         procedure Reraise_Library_Exception_If_Any;
            pragma Import (Ada, Reraise_Library_Exception_If_Any, "__gnat_reraise_library_exception_if_any");
      begin
         Reraise_Library_Exception_If_Any;
      end;
   end finalize_library;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (C, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   type No_Param_Proc is access procedure;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Leap_Seconds_Support : Integer;
      pragma Import (C, Leap_Seconds_Support, "__gl_leap_seconds_support");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");

      Finalize_Library_Objects : No_Param_Proc;
      pragma Import (C, Finalize_Library_Objects, "__gnat_finalize_library_objects");
   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := -1;
      WC_Encoding := 'b';
      Locking_Policy := ' ';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := ' ';
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Detect_Blocking := 0;
      Default_Stack_Size := -1;
      Leap_Seconds_Support := 0;

      Runtime_Initialize (1);

      Finalize_Library_Objects := finalize_library'access;

      System.Soft_Links'Elab_Spec;
      System.Exception_Table'Elab_Body;
      E021 := E021 + 1;
      Ada.Containers'Elab_Spec;
      E150 := E150 + 1;
      Ada.Io_Exceptions'Elab_Spec;
      E065 := E065 + 1;
      Ada.Numerics'Elab_Spec;
      E250 := E250 + 1;
      Ada.Strings'Elab_Spec;
      E215 := E215 + 1;
      Ada.Strings.Maps'Elab_Spec;
      Ada.Strings.Maps.Constants'Elab_Spec;
      E366 := E366 + 1;
      Ada.Tags'Elab_Spec;
      Ada.Streams'Elab_Spec;
      E064 := E064 + 1;
      Interfaces.C'Elab_Spec;
      Interfaces.C.Strings'Elab_Spec;
      System.Exceptions'Elab_Spec;
      E023 := E023 + 1;
      System.File_Control_Block'Elab_Spec;
      E080 := E080 + 1;
      Ada.Streams.Stream_Io'Elab_Spec;
      E179 := E179 + 1;
      System.Finalization_Root'Elab_Spec;
      E073 := E073 + 1;
      Ada.Finalization'Elab_Spec;
      E071 := E071 + 1;
      System.Storage_Pools'Elab_Spec;
      E165 := E165 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Storage_Pools.Subpools'Elab_Spec;
      Ada.Calendar'Elab_Spec;
      Ada.Calendar'Elab_Body;
      E353 := E353 + 1;
      Ada.Calendar.Time_Zones'Elab_Spec;
      E357 := E357 + 1;
      System.Direct_Io'Elab_Spec;
      E375 := E375 + 1;
      System.Pool_Global'Elab_Spec;
      E169 := E169 + 1;
      E173 := E173 + 1;
      System.Finalization_Masters'Elab_Body;
      E159 := E159 + 1;
      System.File_Io'Elab_Body;
      E070 := E070 + 1;
      E088 := E088 + 1;
      E075 := E075 + 1;
      Ada.Tags'Elab_Body;
      E049 := E049 + 1;
      E221 := E221 + 1;
      System.Soft_Links'Elab_Body;
      E011 := E011 + 1;
      System.Os_Lib'Elab_Body;
      E077 := E077 + 1;
      System.Secondary_Stack'Elab_Body;
      E015 := E015 + 1;
      Ada.Strings.Unbounded'Elab_Spec;
      E217 := E217 + 1;
      Ada.Directories'Elab_Spec;
      System.Regexp'Elab_Spec;
      E371 := E371 + 1;
      Ada.Directories'Elab_Body;
      E351 := E351 + 1;
      Ada.Text_Io'Elab_Spec;
      Ada.Text_Io'Elab_Body;
      E062 := E062 + 1;
      GL'ELAB_SPEC;
      GL.TYPES'ELAB_SPEC;
      E112 := E112 + 1;
      GL.CONTEXT'ELAB_SPEC;
      GL.ERRORS'ELAB_SPEC;
      GL.OBJECTS'ELAB_SPEC;
      E147 := E147 + 1;
      GL.OBJECTS.BUFFERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS'ELAB_SPEC;
      GL.OBJECTS.SHADERS.LISTS'ELAB_SPEC;
      E195 := E195 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_SPEC;
      GL.TYPES.COLORS'ELAB_SPEC;
      E130 := E130 + 1;
      GL.FIXED.LIGHTING'ELAB_SPEC;
      GL.IMMEDIATE'ELAB_SPEC;
      GL.OBJECTS.TEXTURES'ELAB_SPEC;
      FT'ELAB_SPEC;
      FT.FACES'ELAB_SPEC;
      E309 := E309 + 1;
      E304 := E304 + 1;
      FT.GLYPHS'ELAB_SPEC;
      E313 := E313 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_SPEC;
      GL.OBJECTS.PROGRAMS'ELAB_SPEC;
      E191 := E191 + 1;
      GL.OBJECTS.FRAMEBUFFERS'ELAB_BODY;
      E183 := E183 + 1;
      GL.OBJECTS.TEXTURES'ELAB_BODY;
      E187 := E187 + 1;
      E296 := E296 + 1;
      E137 := E137 + 1;
      GL.OBJECTS.RENDERBUFFERS'ELAB_BODY;
      E185 := E185 + 1;
      E193 := E193 + 1;
      GL.OBJECTS.BUFFERS'ELAB_BODY;
      E149 := E149 + 1;
      E133 := E133 + 1;
      E231 := E231 + 1;
      GL.OBJECTS.TEXTURES.TARGETS'ELAB_SPEC;
      E316 := E316 + 1;
      E102 := E102 + 1;
      E278 := E278 + 1;
      E286 := E286 + 1;
      E290 := E290 + 1;
      E282 := E282 + 1;
      Geosphere'Elab_Spec;
      E294 := E294 + 1;
      E373 := E373 + 1;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_SPEC;
      GL.OBJECTS.VERTEX_ARRAYS'ELAB_BODY;
      E298 := E298 + 1;
      FT.OGL'ELAB_BODY;
      E311 := E311 + 1;
      E347 := E347 + 1;
      E302 := E302 + 1;
      E300 := E300 + 1;
      Glfw'Elab_Spec;
      Glfw.Input.Joysticks'Elab_Spec;
      Glfw.Monitors'Elab_Spec;
      Glfw.Windows'Elab_Spec;
      Glfw.Api'Elab_Spec;
      E083 := E083 + 1;
      E208 := E208 + 1;
      E100 := E100 + 1;
      E098 := E098 + 1;
      E092 := E092 + 1;
      E090 := E090 + 1;
      E086 := E086 + 1;
      E082 := E082 + 1;
      E212 := E212 + 1;
      Maths'Elab_Spec;
      E276 := E276 + 1;
      E274 := E274 + 1;
      E272 := E272 + 1;
      Program_Loader'Elab_Spec;
      E349 := E349 + 1;
      E255 := E255 + 1;
      E249 := E249 + 1;
      Silo'Elab_Spec;
      Silo'Elab_Body;
      E377 := E377 + 1;
      E214 := E214 + 1;
      E270 := E270 + 1;
      E210 := E210 + 1;
      Ga_Draw'Elab_Body;
      E284 := E284 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_bivectors");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer
   is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      gnat_argc := argc;
      gnat_argv := argv;
      gnat_envp := envp;

      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
      return (gnat_exit_status);
   end;

--  BEGIN Object file/option list
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/ga_maths.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/e3ga.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/e3ga_utilities.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/e2ga_draw.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/geosphere.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/gl_util.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/bivectors.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/multivector_type_base.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/multivector_analysis.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/e2ga.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/program_loader.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/quaternions.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/maths.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/silo.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/utilities.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/main_loop.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/initialize.o
   --   /Education/Geometric_Algebra/GA_Ada/example_2.1/obj/ga_draw.o
   --   -L/Education/Geometric_Algebra/GA_Ada/example_2.1/obj/
   --   -L/Education/Geometric_Algebra/GA_Ada/example_2.1/obj/
   --   -L/Ada_Source/OpenGLAda/lib/
   --   -L/opt/gcc-6.1.0/lib/gcc/x86_64-apple-darwin15/6.1.0/adalib/
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
