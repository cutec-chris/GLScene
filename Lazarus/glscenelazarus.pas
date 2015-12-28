{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit glscenelazarus;

interface

uses
  ApplicationFileIO, ARBProgram, AsyncTimer, BumpMapping, CurvesAndSurfaces, 
  GeometryBB, GLCanvas, GLKeyboard, GLTextureCombiners, HeightTileFile, jpeg, 
  glmeshutils, Octree, OpenGL1x, PerlinNoise, GLPersistentClasses, 
  PictureRegisteredFormats, Polynomials, SpatialPartitioning, Spline, 
  GLVectorGeometry, GLVectorLists, GLVectorTypes, VerletClasses, 
  VerletHairClasses, XCollection, XOpenGL, FVectorEditor, FXCollectionEditor, 
  GLTextureImageEditors, Info, RegisterXCollection, GLFile3DS, GLFileASE, 
  GLFileGL2, GLFileGTS, GLFileLWO, GLFileMD2, GLFileMD3, GLFileMD5, GLFileMDC, 
  GLFileMS3D, GLFileNMF, GLFileNurbs, GLFileObj, GLFilePLY, GLFileSMD, 
  GLFileSTL, GLFileTIN, GLFileVRML, GlFileX, GLVfsPAK, Q3MD3, TGA, Utils3DS, 
  GLAtmosphere, GLBaseMeshSilhouette, GLBehaviours, GLBitmapFont, GLBlur, 
  GLBSP, GLBumpmapHDS, GLCadencer, GLCollision, GLConsole, GLContext, GLDCE, 
  GLDCEMisc, GLEllipseCollision, GLEParticleMasksManager, GLExplosionFx, 
  GLExtrusion, GLFireFX, GLGameMenu, GLGeomObjects, GLGraph, GLGraphics, 
  GLGui, GLHeightData, GLHeightTileFileHDS, GLHUDObjects, GLImposter, 
  GLLensFlare, GLLinePFX, GLMaterialScript, GLMesh, GLMeshCSG, 
  GLMeshOptimizer, GLMirror, GLMovement, GLMultiPolygon, GLMultiProxy, 
  GLObjects, GLParametricSurfaces, GLParticleFX, GLParticles, GLPerlin, 
  GLPerlinBase, GLPerlinPFX, GLPolyhedron, GLPortal, GLPostEffects, 
  GLProcTextures, GLProjectedTextures, GLProxyObjects, GLROAMPatch, 
  glscanlinedgraphics, GLScene, GLScreen, GLShadowPlane, GLShadowVolume, 
  GLSilhouette, GLSkyBox, GLSkydome, GLSLProjectedTextures, GLSound, 
  GLSpatialPartitioning, GLState, GLStrings, GLTeapot, GLTerrainRenderer, 
  GLTexLensFlare, GLTexture, GLThorFX, GLTilePlane, GLTrail, GLUtils, 
  GLVectorFileObjects, GLVerletClasses, GLVerletClothify, 
  GLVerletSkeletonColliders, GLWaterPlane, GLWindows, GLzBuffer, cadencerasap, 
  GLCrossPlatform, GLViewer, GLColor, GLDynamicTexture, GLLCLViewer, GLGizmo, 
  GLFileB3D, GLSLBumpShader, GLSLDiffuseSpecularShader, GLSLPostBlurShader, 
  GLSLShader, GLBaseClasses, GLCoordinates, GLManager, GLNodes, 
  GLRenderContextInfo, GLMaterial, GLObjectManager, GLCameraController, GLFBO, 
  GLFBORenderer, GLGizmoEx, Q3BSP, GLFileMP3, GLFileWAV, GLApplicationFileIO, 
  GLSoundFileObjects, GLLCLFullscreenViewer, GLWidgetContext, GLSceneEditLCL, 
  GLSceneRegisterLCL, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register);
  RegisterUnit('GLSceneRegisterLCL', @GLSceneRegisterLCL.Register);
end;

initialization
  RegisterPackage('glscenelazarus', @Register);
end.
