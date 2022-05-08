
precision highp float;

varying mediump vec4 fColor;
uniform sampler2D texture_diffuse;
varying vec2 f_texture_coordinates;

void main() {
     vec4 textureColor = texture2D(texture_diffuse, f_texture_coordinates);
     gl_FragColor = fColor * textureColor;
}