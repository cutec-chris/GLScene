{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit glscenelazarus;

interface

uses
  ApplicationFileIO, ARBProgram, AsyncTimer, GLCanvas, GLKeyboard, 
  GLTextureCombiners, HeightTileFile, jpeg, OpenGL1x, 
  PictureRegisteredFormats, XCollection, XOpenGL, GLSceneRegister, 
  RegisterXCollection, GLFile3DS, GLFileASE, GLFileGL2, GLFileGTS, GLFileLWO, 
  GLFileMD2, GLFileMD3, GLFileMD5, GLFileMDC, GLFileMS3D, GLFileNMF, 
  GLFileNurbs, GLFileOBJ, GLFilePLY, GLFileSMD, GLFileSTL, GLFileTIN, 
  GLFileVRML, GlFileX, GLVfsPAK, Q3MD3, TGA, Utils3DS, GLAtmosphere, 
  GLBaseMeshSilhouette, GLBehaviours, GLBitmapFont, GLBlur, GLBSP, 
  GLBumpmapHDS, GLCadencer, GLCollision, GLConsole, GLContext, GLDCE, 
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
  GLVerletSkeletonColliders, GLWaterPlane, GLWindows, GLzBuffer, 
  GLCrossPlatform, GLViewer, GLColor, GLDynamicTexture, GLLCLViewer, GLGizmo, 
  GLFileB3D, GLSLBumpShader, GLSLDiffuseSpecularShader, GLSLPostBlurShader, 
  GLSLShader, GLCoordinates, GLManager, GLNodes, GLRenderContextInfo, 
  GLMaterial, GLObjectManager, GLCameraController, GLFBO, GLFBORenderer, 
  GLGizmoEx, Q3BSP, GLFileMP3, GLFileWAV, GLApplicationFileIO, 
  GLSoundFileObjects, GLFileJPEG, GLVectorTypes, GLVectorLists, 
  GLVectorGeometry, GLBumpMapping, GLCurvesAndSurfaces, GLGeometryBB, 
  GLPersistentClasses, GLBaseClasses, OpenGLAdapter, GLPipelineTransformation, 
  GLMeshUtils, GLOctree, GLPerlinNoise3D, GLPolynomials, GLSpline, 
  GLVerletHairClasses, GLImageUtils, GLSelection, GLMaterialEx, 
  GLFullScreenViewer, GLSLParameter, GLAsyncTimer, GLSceneForm, 
  GLSArchiveManager, FXCollectionEditorLCL, FVectorEditorLCL, 
  FShaderUniformEditorLCL, FRTrackBarEditLCL, FRTextureEditLCL, 
  FRMaterialPreviewLCL, FRFaceEditorLCL, FRColorEditorLCL, 
  FMaterialEditorFormLCL, FLibMaterialPickerLCL, FInfoLCL, GLSpacePartition, 
  GLVerletTypes, GLSceneEditLCL, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSceneRegister', @GLSceneRegister.Register);
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register);
end;

initialization
  RegisterPackage('glscenelazarus', @Register);
end.
