#version 410

uniform vec3 vector_colour;

out vec3 fragment_colour;

void main()
{
   fragment_colour = vector_colour;
}
