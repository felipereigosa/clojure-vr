
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;
uniform mat4 modelMatrix;
uniform mat4 cameraMatrix;
attribute vec4 position;
uniform vec4 materialColor;
varying mediump vec4 fColor;

void main() {
    gl_Position = projectionMatrix * viewMatrix * cameraMatrix * modelMatrix * position;
    fColor = materialColor;
}