import * as THREE from 'three';
import {GLTFLoader} from 'GLTFLoader';
import {VRButton} from 'VRButton';
import {OrbitControls} from 'OrbitControls';
import * as CANNON from './cannon-es.js'

window.THREE = THREE;
window.VRButton = VRButton;
window.GLTFLoader = GLTFLoader;
window.OrbitControls = OrbitControls;
window.CANNON = CANNON
