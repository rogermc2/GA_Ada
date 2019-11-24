--   part of OpenGLAda, (c) 2017 Felix Krause
--   released under the terms of the MIT license, see the file "COPYING"

private with GL.Low_Level;

package GL_Enums_Feedback is
   pragma Preelaborate;

   type Feed_Back_Mode is (Feed_Back_2D, Feed_back_3D , Color_3D,
                           Color_Texture_3D, Color_Texture_4D);

   type Feed_Back_Token is (Pass_Through_Token, Point_Token, Line_Token,
                            Polygon_Token, Bitmap_Token,  Draw_Pixel_Token,
                            Copy_Pixel_Token, Line_Reset_Token);

private

   for Feed_Back_Mode use (Feed_Back_2D      => 16#0600#,
                           Feed_back_3D      => 16#0601#,
                           Color_3D          => 16#0602#,
                           Color_Texture_3D  => 16#0603#,
                           Color_Texture_4D  => 16#0604#);
   for Feed_Back_Mode'Size use GL.Low_Level.Enum'Size;

   for Feed_Back_Token use (Pass_Through_Token  => 16#0700#,
                            Point_Token         => 16#0701#,
                            Line_Token          => 16#0702#,
                            Polygon_Token       => 16#0703#,
                            Bitmap_Token        => 16#0704#,
                            Draw_Pixel_Token    => 16#0705#,
                            Copy_Pixel_Token    => 16#0706#,
                            Line_Reset_Token    => 16#0707#);
   for Feed_Back_Token'Size use GL.Low_Level.Enum'Size;

end GL_Enums_Feedback;
