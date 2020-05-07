#version 410 core

out vec4 fragment_colour;

uniform vec4 Drawing_Colour;

void main()
{
   //  fragment_colour = vec4(0.5, 0.0, 0.5, 1.0);
    fragment_colour = Drawing_Colour;
}
