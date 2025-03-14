//***************************************************************************************
// DeferApp.cpp by Frank Luna (C) 2015 All Rights Reserved.
//***************************************************************************************

#include <MyDX12/UploadBuffer.h>
#include <memory>
#include "../common/GeometryGenerator.h"
#include "../common/MathHelper.h"
#include "../common/d3dApp.h"

using Microsoft::WRL::ComPtr;
using namespace DirectX;
using namespace DirectX::PackedVector;

const int gNumFrameResources = 3;

struct ObjectConstants {
  DirectX::XMFLOAT4X4 World = MathHelper::Identity4x4();
  DirectX::XMFLOAT4X4 TexTransform = MathHelper::Identity4x4();
};

struct PassConstants {
  DirectX::XMFLOAT4X4 View = MathHelper::Identity4x4();
  DirectX::XMFLOAT4X4 InvView = MathHelper::Identity4x4();
  DirectX::XMFLOAT4X4 Proj = MathHelper::Identity4x4();
  DirectX::XMFLOAT4X4 InvProj = MathHelper::Identity4x4();
  DirectX::XMFLOAT4X4 ViewProj = MathHelper::Identity4x4();
  DirectX::XMFLOAT4X4 InvViewProj = MathHelper::Identity4x4();
  DirectX::XMFLOAT3 EyePosW = {0.0f, 0.0f, 0.0f};
  float cbPerObjectPad1 = 0.0f;
  DirectX::XMFLOAT2 RenderTargetSize = {0.0f, 0.0f};
  DirectX::XMFLOAT2 InvRenderTargetSize = {0.0f, 0.0f};
  float NearZ = 0.0f;
  float FarZ = 0.0f;
  float TotalTime = 0.0f;
  float DeltaTime = 0.0f;

  DirectX::XMFLOAT4 AmbientLight = {0.0f, 0.0f, 0.0f, 1.0f};

  // Indices [0, NUM_DIR_LIGHTS) are directional lights;
  // indices [NUM_DIR_LIGHTS, NUM_DIR_LIGHTS+NUM_POINT_LIGHTS) are point lights;
  // indices [NUM_DIR_LIGHTS+NUM_POINT_LIGHTS, NUM_DIR_LIGHTS+NUM_POINT_LIGHT+NUM_SPOT_LIGHTS)
  // are spot lights for a maximum of MaxLights per object.
  Light Lights[MaxLights];
};

struct Vertex {
  DirectX::XMFLOAT3 Pos;
  DirectX::XMFLOAT3 Normal;
  DirectX::XMFLOAT2 TexC;
};

// Lightweight structure stores parameters to draw a shape.  This will
// vary from app-to-app.
struct RenderItem {
  RenderItem() = default;

  // World matrix of the shape that describes the object's local space
  // relative to the world space, which defines the position, orientation,
  // and scale of the object in the world.
  XMFLOAT4X4 World = MathHelper::Identity4x4();

  XMFLOAT4X4 TexTransform = MathHelper::Identity4x4();

  // Dirty flag indicating the object data has changed and we need to update the constant buffer.
  // Because we have an object cbuffer for each FrameResource, we have to apply the
  // update to each FrameResource.  Thus, when we modify obect data we should set
  // NumFramesDirty = gNumFrameResources so that each frame resource gets the update.
  int NumFramesDirty = gNumFrameResources;

  // Index into GPU constant buffer corresponding to the ObjectCB for this render item.
  UINT ObjCBIndex = -1;

  Material* Mat = nullptr;
  My::MyDX12::MeshGeometry* Geo = nullptr;
  //std::string Geo;

  // Primitive topology.
  D3D12_PRIMITIVE_TOPOLOGY PrimitiveType =
      D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST;

  // DrawIndexedInstanced parameters.
  UINT IndexCount = 0;
  UINT StartIndexLocation = 0;
  int BaseVertexLocation = 0;
};

class DeferApp : public D3DApp {
 public:
  DeferApp(HINSTANCE hInstance);
  DeferApp(const DeferApp& rhs) = delete;
  DeferApp& operator=(const DeferApp& rhs) = delete;
  ~DeferApp();

  virtual bool Initialize() override;

 private:
  virtual void OnResize() override;
  virtual void Update(const GameTimer& gt) override;
  virtual void Draw(const GameTimer& gt) override;

  virtual void OnMouseDown(WPARAM btnState, int x, int y) override;
  virtual void OnMouseUp(WPARAM btnState, int x, int y) override;
  virtual void OnMouseMove(WPARAM btnState, int x, int y) override;

  void OnKeyboardInput(const GameTimer& gt);
  void UpdateCamera(const GameTimer& gt);
  void AnimateMaterials(const GameTimer& gt);
  void UpdateObjectCBs(const GameTimer& gt);
  void UpdateMaterialCBs(const GameTimer& gt);
  void UpdateMainPassCB(const GameTimer& gt);

  void LoadTextures();
  void BuildRootSignature();
  void BuildDescriptorHeaps();
  void BuildShadersAndInputLayout();
  void BuildShapeGeometry();
  void BuildPSOs();
  void BuildFrameResources();
  void BuildMaterials();
  void BuildRenderItems();
  void DrawRenderItems(ID3D12GraphicsCommandList* cmdList,
                       const std::vector<RenderItem*>& ritems);

 private:
  std::vector<std::unique_ptr<My::MyDX12::FrameRsrcMngr>> mFrameResources;
  My::MyDX12::FrameRsrcMngr* mCurrFrameRsrcMngr = nullptr;
  int mCurrFrameRsrcMngrIndex = 0;

  //UINT mCbvSrvDescriptorSize = 0;

  //ComPtr<ID3D12RootSignature> mRootSignature = nullptr;

  //ComPtr<ID3D12DescriptorHeap> mSrvDescriptorHeap = nullptr;
  My::MyDX12::DescriptorHeapAllocation mSrvDescriptorHeap;

  //std::unordered_map<std::string, std::unique_ptr<My::MyDX12::MeshGeometry>> mGeometries;
  std::unordered_map<std::string, std::unique_ptr<Material>> mMaterials;
  std::unordered_map<std::string, std::unique_ptr<Texture>> mTextures;
  //std::unordered_map<std::string, ComPtr<ID3DBlob>> mShaders;

  std::vector<D3D12_INPUT_ELEMENT_DESC> mInputLayout;

  //ComPtr<ID3D12PipelineState> mOpaquePSO = nullptr;

  // List of all the render items.
  std::vector<std::unique_ptr<RenderItem>> mAllRitems;

  // Render items divided by PSO.
  std::vector<RenderItem*> mOpaqueRitems;

  PassConstants mMainPassCB;

  XMFLOAT3 mEyePos = {0.0f, 0.0f, 0.0f};
  XMFLOAT4X4 mView = MathHelper::Identity4x4();
  XMFLOAT4X4 mProj = MathHelper::Identity4x4();

  float mTheta = 1.3f * XM_PI;
  float mPhi = 0.4f * XM_PI;
  float mRadius = 2.5f;

  POINT mLastMousePos;

  // frame graph
  //My::MyDX12::FG::RsrcMngr fgRsrcMngr;
  My::MyDX12::FG::Executor fgExecutor;
  My::MyFG::Compiler fgCompiler;
  My::MyFG::FrameGraph fg;
};

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE prevInstance, PSTR cmdLine,
                   int showCmd) {
  // Enable run-time memory check for debug builds.
#if defined(DEBUG) | defined(_DEBUG)
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

  try {
    DeferApp theApp(hInstance);
    if (!theApp.Initialize())
      return 0;

    int rst = theApp.Run();
    My::DXRenderer::Instance().Release();
    return rst;
  } catch (My::MyDX12::Util::Exception& e) {
    MessageBox(nullptr, e.ToString().c_str(), L"HR Failed", MB_OK);
    return 0;
  }
}

DeferApp::DeferApp(HINSTANCE hInstance)
    : D3DApp(hInstance), fg{"frame graph"} {}

DeferApp::~DeferApp() {
  if (!myDevice.IsNull())
    FlushCommandQueue();
}

bool DeferApp::Initialize() {
  if (!D3DApp::Initialize())
    return false;

  My::DXRenderer::Instance().Init(myDevice.raw.Get());

  My::MyDX12::DescriptorHeapMngr::Instance().Init(myDevice.raw.Get(), 1024,
                                                  1024, 1024, 1024, 1024);

  //fgRsrcMngr.Init(myGCmdList, myDevice);

  // Reset the command list to prep for initialization commands.
  ThrowIfFailed(myGCmdList->Reset(mDirectCmdListAlloc.Get(), nullptr));

  // Get the increment size of a descriptor in this heap type.  This is hardware specific,
  // so we have to query this information.
  //mCbvSrvDescriptorSize = myDevice->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

  My::DXRenderer::Instance().GetUpload().Begin();

  LoadTextures();
  BuildRootSignature();
  BuildDescriptorHeaps();
  BuildShadersAndInputLayout();
  BuildShapeGeometry();
  BuildMaterials();
  BuildRenderItems();
  BuildFrameResources();
  BuildPSOs();

  // Execute the initialization commands.
  ThrowIfFailed(myGCmdList->Close());
  myCmdQueue.Execute(myGCmdList.raw.Get());

  My::DXRenderer::Instance().GetUpload().End(myCmdQueue.raw.Get());

  // Wait until initialization is complete.
  FlushCommandQueue();

  return true;
}

void DeferApp::OnResize() {
  D3DApp::OnResize();

  // The window resized, so update the aspect ratio and recompute the projection matrix.
  XMMATRIX P = XMMatrixPerspectiveFovLH(0.25f * MathHelper::Pi, AspectRatio(),
                                        1.0f, 1000.0f);
  XMStoreFloat4x4(&mProj, P);

  auto clearFGRsrcMngr =
      [](std::shared_ptr<My::MyDX12::FG::RsrcMngr> rsrcMngr) {
        rsrcMngr->Clear();
      };
  for (auto& frsrc : mFrameResources)
    frsrc->DelayUpdateResource("FrameGraphRsrcMngr", clearFGRsrcMngr);
}

void DeferApp::Update(const GameTimer& gt) {
  OnKeyboardInput(gt);
  UpdateCamera(gt);

  // Cycle through the circular frame resource array.
  mCurrFrameRsrcMngrIndex = (mCurrFrameRsrcMngrIndex + 1) % gNumFrameResources;
  mCurrFrameRsrcMngr = mFrameResources[mCurrFrameRsrcMngrIndex].get();

  // Has the GPU finished processing the commands of the current frame resource?
  // If not, wait until the GPU has completed commands up to this fence point.
  mCurrFrameRsrcMngr->Wait();
  /*if(mCurrFrameRsrcMngr->Fence != 0 && mFence->GetCompletedValue() < mCurrFrameRsrcMngr->Fence)
    {
        HANDLE eventHandle = CreateEventEx(nullptr, false, false, EVENT_ALL_ACCESS);
        ThrowIfFailed(mFence->SetEventOnCompletion(mCurrFrameRsrcMngr->Fence, eventHandle));
        WaitForSingleObject(eventHandle, INFINITE);
        CloseHandle(eventHandle);
    }*/

  AnimateMaterials(gt);
  UpdateObjectCBs(gt);
  UpdateMaterialCBs(gt);
  UpdateMainPassCB(gt);
}

void DeferApp::Draw(const GameTimer& gt) {
  auto cmdListAlloc =
      mCurrFrameRsrcMngr
          ->GetResource<Microsoft::WRL::ComPtr<ID3D12CommandAllocator>>(
              "CommandAllocator");

  // Reuse the memory associated with command recording.
  // We can only reset when the associated command lists have finished execution on the GPU.
  ThrowIfFailed(cmdListAlloc->Reset());

  // A command list can be reset after it has been added to the command queue via ExecuteCommandList.
  // Reusing the command list reuses memory.
  ThrowIfFailed(myGCmdList->Reset(cmdListAlloc.Get(),
                                  My::DXRenderer::Instance().GetPSO("opaque")));

  myGCmdList.SetDescriptorHeaps(My::MyDX12::DescriptorHeapMngr::Instance()
                                    .GetCSUGpuDH()
                                    ->GetDescriptorHeap());
  myGCmdList->RSSetViewports(1, &mScreenViewport);
  myGCmdList->RSSetScissorRects(1, &mScissorRect);

  fg.Clear();
  auto fgRsrcMngr =
      mCurrFrameRsrcMngr
          ->GetResource<std::shared_ptr<My::MyDX12::FG::RsrcMngr>>(
              "FrameGraphRsrcMngr");
  fgRsrcMngr->NewFrame();
  fgExecutor.NewFrame();
  ;

  auto backbuffer = fg.RegisterResourceNode("Back Buffer");
  auto depthstencil = fg.RegisterResourceNode("Depth Stencil");
  auto pass = fg.RegisterPassNode("Pass", {}, {backbuffer, depthstencil});

  D3D12_DEPTH_STENCIL_VIEW_DESC dsvDesc;
  dsvDesc.Flags = D3D12_DSV_FLAG_NONE;
  dsvDesc.ViewDimension = D3D12_DSV_DIMENSION_TEXTURE2D;
  dsvDesc.Format = mDepthStencilFormat;
  dsvDesc.Texture2D.MipSlice = 0;

  (*fgRsrcMngr)
      .RegisterImportedRsrc(backbuffer,
                            {CurrentBackBuffer(), D3D12_RESOURCE_STATE_PRESENT})
      .RegisterImportedRsrc(depthstencil, {mDepthStencilBuffer.Get(),
                                           D3D12_RESOURCE_STATE_DEPTH_WRITE})
      .RegisterPassRsrcs(pass, backbuffer, D3D12_RESOURCE_STATE_RENDER_TARGET,
                         My::MyDX12::FG::RsrcImplDesc_RTV_Null{})
      .RegisterPassRsrcs(pass, depthstencil, D3D12_RESOURCE_STATE_DEPTH_WRITE,
                         dsvDesc);

  fgExecutor.RegisterPassFunc(
      pass, [&](const My::MyDX12::FG::PassRsrcs& rsrcs) {
        // Clear the back buffer and depth buffer.
        myGCmdList.ClearRenderTargetView(
            rsrcs.find(backbuffer)->second.cpuHandle, Colors::LightSteelBlue);
        myGCmdList.ClearDepthStencilView(
            rsrcs.find(depthstencil)->second.cpuHandle);

        // Specify the buffers we are going to render to.
        myGCmdList.OMSetRenderTarget(
            rsrcs.find(backbuffer)->second.cpuHandle,
            rsrcs.find(depthstencil)->second.cpuHandle);

        //myGCmdList.SetDescriptorHeaps(mSrvDescriptorHeap.Get());

        myGCmdList->SetGraphicsRootSignature(
            My::DXRenderer::Instance().GetRootSignature("default"));

        auto passCB =
            mCurrFrameRsrcMngr
                ->GetResource<My::MyDX12::ArrayUploadBuffer<PassConstants>>(
                    "ArrayUploadBuffer<PassConstants>")
                .GetResource();
        myGCmdList->SetGraphicsRootConstantBufferView(
            2, passCB->GetGPUVirtualAddress());

        DrawRenderItems(myGCmdList.raw.Get(), mOpaqueRitems);

        DrawRenderItems(myGCmdList.raw.Get(), mOpaqueRitems);
      });

  //   // Indicate a state transition on the resource usage.
  //myGCmdList->ResourceBarrier(1, &CD3DX12_RESOURCE_BARRIER::Transition(CurrentBackBuffer(),
  //	D3D12_RESOURCE_STATE_PRESENT, D3D12_RESOURCE_STATE_RENDER_TARGET));

  //   // Clear the back buffer and depth buffer.
  //   myGCmdList->ClearRenderTargetView(CurrentBackBufferView(), Colors::LightSteelBlue, 0, nullptr);
  //   myGCmdList->ClearDepthStencilView(DepthStencilView(), D3D12_CLEAR_FLAG_DEPTH | D3D12_CLEAR_FLAG_STENCIL, 1.0f, 0, 0, nullptr);

  //   // Specify the buffers we are going to render to.
  //   myGCmdList->OMSetRenderTargets(1, &CurrentBackBufferView(), true, &DepthStencilView());

  //ID3D12DescriptorHeap* descriptorHeaps[] = { mSrvDescriptorHeap.Get() };
  //myGCmdList->SetDescriptorHeaps(_countof(descriptorHeaps), descriptorHeaps);

  //myGCmdList->SetGraphicsRootSignature(mRootSignature.Get());

  //auto passCB = mCurrFrameRsrcMngr->PassCB->GetResource();
  //myGCmdList->SetGraphicsRootConstantBufferView(2, passCB->GetGPUVirtualAddress());

  //   DrawRenderItems(myGCmdList.raw.Get(), mOpaqueRitems);

  //   // Indicate a state transition on the resource usage.
  //myGCmdList->ResourceBarrier(1, &CD3DX12_RESOURCE_BARRIER::Transition(CurrentBackBuffer(),
  //	D3D12_RESOURCE_STATE_RENDER_TARGET, D3D12_RESOURCE_STATE_PRESENT));

  auto [success, crst] = fgCompiler.Compile(fg);
  fgExecutor.Execute(crst, *fgRsrcMngr);

  // Done recording commands.
  ThrowIfFailed(myGCmdList->Close());

  // Add the command list to the queue for execution.
  myCmdQueue.Execute(myGCmdList.raw.Get());

  // Swap the back and front buffers
  ThrowIfFailed(mSwapChain->Present(0, 0));
  mCurrBackBuffer = (mCurrBackBuffer + 1) % SwapChainBufferCount;

  //// Advance the fence value to mark commands up to this fence point.
  //mCurrFrameRsrcMngr->Fence = ++mCurrentFence;

  //// Add an instruction to the command queue to set a new fence point.
  //// Because we are on the GPU timeline, the new fence point won't be
  //// set until the GPU finishes processing all the commands prior to this Signal().
  //myCmdQueue->Signal(mFence.Get(), mCurrentFence);
  mCurrFrameRsrcMngr->Signal(myCmdQueue.raw.Get(), ++mCurrentFence);
}

void DeferApp::OnMouseDown(WPARAM btnState, int x, int y) {
  mLastMousePos.x = x;
  mLastMousePos.y = y;

  SetCapture(mhMainWnd);
}

void DeferApp::OnMouseUp(WPARAM btnState, int x, int y) {
  ReleaseCapture();
}

void DeferApp::OnMouseMove(WPARAM btnState, int x, int y) {
  if ((btnState & MK_LBUTTON) != 0) {
    // Make each pixel correspond to a quarter of a degree.
    float dx =
        XMConvertToRadians(0.25f * static_cast<float>(x - mLastMousePos.x));
    float dy =
        XMConvertToRadians(0.25f * static_cast<float>(y - mLastMousePos.y));

    // Update angles based on input to orbit camera around box.
    mTheta += dx;
    mPhi += dy;

    // Restrict the angle mPhi.
    mPhi = MathHelper::Clamp(mPhi, 0.1f, MathHelper::Pi - 0.1f);
  } else if ((btnState & MK_RBUTTON) != 0) {
    // Make each pixel correspond to 0.2 unit in the scene.
    float dx = 0.05f * static_cast<float>(x - mLastMousePos.x);
    float dy = 0.05f * static_cast<float>(y - mLastMousePos.y);

    // Update the camera radius based on input.
    mRadius += dx - dy;

    // Restrict the radius.
    mRadius = MathHelper::Clamp(mRadius, 5.0f, 150.0f);
  }

  mLastMousePos.x = x;
  mLastMousePos.y = y;
}

void DeferApp::OnKeyboardInput(const GameTimer& gt) {}

void DeferApp::UpdateCamera(const GameTimer& gt) {
  // Convert Spherical to Cartesian coordinates.
  mEyePos.x = mRadius * sinf(mPhi) * cosf(mTheta);
  mEyePos.z = mRadius * sinf(mPhi) * sinf(mTheta);
  mEyePos.y = mRadius * cosf(mPhi);

  // Build the view matrix.
  XMVECTOR pos = XMVectorSet(mEyePos.x, mEyePos.y, mEyePos.z, 1.0f);
  XMVECTOR target = XMVectorZero();
  XMVECTOR up = XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f);

  XMMATRIX view = XMMatrixLookAtLH(pos, target, up);
  XMStoreFloat4x4(&mView, view);
}

void DeferApp::AnimateMaterials(const GameTimer& gt) {}

void DeferApp::UpdateObjectCBs(const GameTimer& gt) {
  auto& currObjectCB =
      mCurrFrameRsrcMngr
          ->GetResource<My::MyDX12::ArrayUploadBuffer<ObjectConstants>>(
              "ArrayUploadBuffer<ObjectConstants>");
  for (auto& e : mAllRitems) {
    // Only update the cbuffer data if the constants have changed.
    // This needs to be tracked per frame resource.
    if (e->NumFramesDirty > 0) {
      XMMATRIX world = XMLoadFloat4x4(&e->World);
      XMMATRIX texTransform = XMLoadFloat4x4(&e->TexTransform);

      ObjectConstants objConstants;
      XMStoreFloat4x4(&objConstants.World, XMMatrixTranspose(world));
      XMStoreFloat4x4(&objConstants.TexTransform,
                      XMMatrixTranspose(texTransform));

      currObjectCB.Set(e->ObjCBIndex, objConstants);

      // Next FrameResource need to be updated too.
      e->NumFramesDirty--;
    }
  }
}

void DeferApp::UpdateMaterialCBs(const GameTimer& gt) {
  auto& currMaterialCB =
      mCurrFrameRsrcMngr
          ->GetResource<My::MyDX12::ArrayUploadBuffer<MaterialConstants>>(
              "ArrayUploadBuffer<MaterialConstants>");
  for (auto& e : mMaterials) {
    // Only update the cbuffer data if the constants have changed.  If the cbuffer
    // data changes, it needs to be updated for each FrameResource.
    Material* mat = e.second.get();
    if (mat->NumFramesDirty > 0) {
      XMMATRIX matTransform = XMLoadFloat4x4(&mat->MatTransform);

      MaterialConstants matConstants;
      matConstants.DiffuseAlbedo = mat->DiffuseAlbedo;
      matConstants.FresnelR0 = mat->FresnelR0;
      matConstants.Roughness = mat->Roughness;
      XMStoreFloat4x4(&matConstants.MatTransform,
                      XMMatrixTranspose(matTransform));

      currMaterialCB.Set(mat->MatCBIndex, matConstants);

      // Next FrameResource need to be updated too.
      mat->NumFramesDirty--;
    }
  }
}

void DeferApp::UpdateMainPassCB(const GameTimer& gt) {
  XMMATRIX view = XMLoadFloat4x4(&mView);
  XMMATRIX proj = XMLoadFloat4x4(&mProj);

  XMMATRIX viewProj = XMMatrixMultiply(view, proj);
  XMMATRIX invView = XMMatrixInverse(&XMMatrixDeterminant(view), view);
  XMMATRIX invProj = XMMatrixInverse(&XMMatrixDeterminant(proj), proj);
  XMMATRIX invViewProj =
      XMMatrixInverse(&XMMatrixDeterminant(viewProj), viewProj);

  XMStoreFloat4x4(&mMainPassCB.View, XMMatrixTranspose(view));
  XMStoreFloat4x4(&mMainPassCB.InvView, XMMatrixTranspose(invView));
  XMStoreFloat4x4(&mMainPassCB.Proj, XMMatrixTranspose(proj));
  XMStoreFloat4x4(&mMainPassCB.InvProj, XMMatrixTranspose(invProj));
  XMStoreFloat4x4(&mMainPassCB.ViewProj, XMMatrixTranspose(viewProj));
  XMStoreFloat4x4(&mMainPassCB.InvViewProj, XMMatrixTranspose(invViewProj));
  mMainPassCB.EyePosW = mEyePos;
  mMainPassCB.RenderTargetSize =
      XMFLOAT2((float)mClientWidth, (float)mClientHeight);
  mMainPassCB.InvRenderTargetSize =
      XMFLOAT2(1.0f / mClientWidth, 1.0f / mClientHeight);
  mMainPassCB.NearZ = 1.0f;
  mMainPassCB.FarZ = 1000.0f;
  mMainPassCB.TotalTime = gt.TotalTime();
  mMainPassCB.DeltaTime = gt.DeltaTime();
  mMainPassCB.AmbientLight = {0.25f, 0.25f, 0.35f, 1.0f};
  mMainPassCB.Lights[0].Direction = {0.57735f, -0.57735f, 0.57735f};
  mMainPassCB.Lights[0].Strength = {0.6f, 0.6f, 0.6f};
  mMainPassCB.Lights[1].Direction = {-0.57735f, -0.57735f, 0.57735f};
  mMainPassCB.Lights[1].Strength = {0.3f, 0.3f, 0.3f};
  mMainPassCB.Lights[2].Direction = {0.0f, -0.707f, -0.707f};
  mMainPassCB.Lights[2].Strength = {0.15f, 0.15f, 0.15f};

  auto& currPassCB =
      mCurrFrameRsrcMngr
          ->GetResource<My::MyDX12::ArrayUploadBuffer<PassConstants>>(
              "ArrayUploadBuffer<PassConstants>");
  currPassCB.Set(0, mMainPassCB);
}

void DeferApp::LoadTextures() {
  /*auto woodCrateTex = std::make_unique<Texture>();
	woodCrateTex->Name = "woodCrateTex";
	woodCrateTex->Filename = L"../data/textures/WoodCrate01.dds";
	ThrowIfFailed(DirectX::CreateDDSTextureFromFile12(myDevice.raw.Get(),
		myGCmdList.raw.Get(), woodCrateTex->Filename.c_str(),
		woodCrateTex->Resource, woodCrateTex->UploadHeap));
 
	mTextures[woodCrateTex->Name] = std::move(woodCrateTex);*/
  My::DXRenderer::Instance().RegisterDDSTextureFromFile(
      My::DXRenderer::Instance().GetUpload(), "woodCrateTex",
      L"../data/textures/WoodCrate01.dds");
}

void DeferApp::BuildRootSignature() {
  CD3DX12_DESCRIPTOR_RANGE texTable;
  texTable.Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0);

  // Root parameter can be a table, root descriptor or root constants.
  CD3DX12_ROOT_PARAMETER slotRootParameter[4];

  // Perfomance TIP: Order from most frequent to least frequent.
  slotRootParameter[0].InitAsDescriptorTable(1, &texTable,
                                             D3D12_SHADER_VISIBILITY_PIXEL);
  //slotRootParameter[0].InitAsShaderResourceView(0, 0, D3D12_SHADER_VISIBILITY_PIXEL);
  slotRootParameter[1].InitAsConstantBufferView(0);
  slotRootParameter[2].InitAsConstantBufferView(1);
  slotRootParameter[3].InitAsConstantBufferView(2);

  auto staticSamplers = My::DXRenderer::Instance().GetStaticSamplers();

  // A root signature is an array of root parameters.
  CD3DX12_ROOT_SIGNATURE_DESC rootSigDesc(
      4, slotRootParameter, (UINT)staticSamplers.size(), staticSamplers.data(),
      D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

  // create a root signature with a single slot which points to a descriptor range consisting of a single constant buffer
  /*ComPtr<ID3DBlob> serializedRootSig = nullptr;
    ComPtr<ID3DBlob> errorBlob = nullptr;
    HRESULT hr = D3D12SerializeRootSignature(&rootSigDesc, D3D_ROOT_SIGNATURE_VERSION_1,
        serializedRootSig.GetAddressOf(), errorBlob.GetAddressOf());

    if(errorBlob != nullptr)
    {
        ::OutputDebugStringA((char*)errorBlob->GetBufferPointer());
    }
    ThrowIfFailed(hr);

    ThrowIfFailed(myDevice->CreateRootSignature(
		0,
        serializedRootSig->GetBufferPointer(),
        serializedRootSig->GetBufferSize(),
        IID_PPV_ARGS(mRootSignature.GetAddressOf())));*/

  My::DXRenderer::Instance().RegisterRootSignature("default", &rootSigDesc);
}

void DeferApp::BuildDescriptorHeaps() {
  //
  // Create the SRV heap.
  //
  /*D3D12_DESCRIPTOR_HEAP_DESC srvHeapDesc = {};
	srvHeapDesc.NumDescriptors = 1;
	srvHeapDesc.Type = D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV;
	srvHeapDesc.Flags = D3D12_DESCRIPTOR_HEAP_FLAG_SHADER_VISIBLE;
	ThrowIfFailed(myDevice->CreateDescriptorHeap(&srvHeapDesc, IID_PPV_ARGS(&mSrvDescriptorHeap)));*/

  //mSrvDescriptorHeap = My::MyDX12::DescriptorHeapMngr::Instance().GetCSUGpuDH()->Allocate(1);

  ////
  //// Fill out the heap with actual descriptors.
  ////
  ///*CD3DX12_CPU_DESCRIPTOR_HANDLE hDescriptor(mSrvDescriptorHeap->GetCPUDescriptorHandleForHeapStart());*/
  //CD3DX12_CPU_DESCRIPTOR_HANDLE hDescriptor(mSrvDescriptorHeap.GetCpuHandle());

  //auto woodCrateTex = mTextures["woodCrateTex"]->Resource;
  //
  //myDevice.CreateSRV_Tex2D(woodCrateTex.Get(), hDescriptor);
}

void DeferApp::BuildShadersAndInputLayout() {
  //mShaders["standardVS"] = My::MyDX12::Util::CompileShader(L"..\\data\\shaders\\00_crate\\Default.hlsl", nullptr, "VS", "vs_5_0");
  //mShaders["opaquePS"] = My::MyDX12::Util::CompileShader(L"..\\data\\shaders\\00_crate\\Default.hlsl", nullptr, "PS", "ps_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "standardVS", L"..\\data\\shaders\\00_crate\\Default.hlsl", nullptr, "VS",
      "vs_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "opaquePS", L"..\\data\\shaders\\00_crate\\Default.hlsl", nullptr, "PS",
      "ps_5_0");

  mInputLayout = {
      {"POSITION", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 0,
       D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
      {"NORMAL", 0, DXGI_FORMAT_R32G32B32_FLOAT, 0, 12,
       D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
      {"TEXCOORD", 0, DXGI_FORMAT_R32G32_FLOAT, 0, 24,
       D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA, 0},
  };
}

void DeferApp::BuildShapeGeometry() {
  GeometryGenerator geoGen;
  GeometryGenerator::MeshData box = geoGen.CreateBox(1.0f, 1.0f, 1.0f, 3);

  My::MyDX12::SubmeshGeometry boxSubmesh;
  boxSubmesh.IndexCount = (UINT)box.Indices32.size();
  boxSubmesh.StartIndexLocation = 0;
  boxSubmesh.BaseVertexLocation = 0;

  std::vector<Vertex> vertices(box.Vertices.size());

  for (size_t i = 0; i < box.Vertices.size(); ++i) {
    vertices[i].Pos = box.Vertices[i].Position;
    vertices[i].Normal = box.Vertices[i].Normal;
    vertices[i].TexC = box.Vertices[i].TexC;
  }

  std::vector<std::uint16_t> indices = box.GetIndices16();

  const UINT vbByteSize = (UINT)vertices.size() * sizeof(Vertex);
  const UINT ibByteSize = (UINT)indices.size() * sizeof(std::uint16_t);

  /*auto geo = std::make_unique<My::MyDX12::MeshGeometry>();
	geo->Name = "boxGeo";

	ThrowIfFailed(D3DCreateBlob(vbByteSize, &geo->VertexBufferCPU));
	CopyMemory(geo->VertexBufferCPU->GetBufferPointer(), vertices.data(), vbByteSize);

	ThrowIfFailed(D3DCreateBlob(ibByteSize, &geo->IndexBufferCPU));
	CopyMemory(geo->IndexBufferCPU->GetBufferPointer(), indices.data(), ibByteSize);

	geo->VertexBufferGPU = My::MyDX12::Util::CreateDefaultBuffer(myDevice.raw.Get(),
		myGCmdList.raw.Get(), vertices.data(), vbByteSize, geo->VertexBufferUploader);

	geo->IndexBufferGPU = My::MyDX12::Util::CreateDefaultBuffer(myDevice.raw.Get(),
		myGCmdList.raw.Get(), indices.data(), ibByteSize, geo->IndexBufferUploader);

	geo->VertexByteStride = sizeof(Vertex);
	geo->VertexBufferByteSize = vbByteSize;
	geo->IndexFormat = DXGI_FORMAT_R16_UINT;
	geo->IndexBufferByteSize = ibByteSize;*/

  /*geo->InitBuffer(myDevice.raw.Get(), My::DXRenderer::Instance().GetUpload(),
		vertices.data(), (UINT)vertices.size(), sizeof(Vertex),
		indices.data(), (UINT)indices.size(), DXGI_FORMAT_R16_UINT);

	geo->submeshGeometries["box"] = boxSubmesh;

	mGeometries[geo->Name] = std::move(geo);*/

  My::DXRenderer::Instance()
      .RegisterStaticMeshGeometry(
          My::DXRenderer::Instance().GetUpload(), "boxGeo", vertices.data(),
          (UINT)vertices.size(), sizeof(Vertex), indices.data(),
          (UINT)indices.size(), DXGI_FORMAT_R16_UINT)
      .submeshGeometries["box"] = boxSubmesh;
}

void DeferApp::BuildPSOs() {
  //   D3D12_GRAPHICS_PIPELINE_STATE_DESC opaquePsoDesc;

  ////
  //// PSO for opaque objects.
  ////
  //   ZeroMemory(&opaquePsoDesc, sizeof(D3D12_GRAPHICS_PIPELINE_STATE_DESC));
  //opaquePsoDesc.InputLayout = { mInputLayout.data(), (UINT)mInputLayout.size() };
  //opaquePsoDesc.pRootSignature = mRootSignature.Get();
  //opaquePsoDesc.VS =
  //{
  //	reinterpret_cast<BYTE*>(My::DXRenderer::Instance().GetShaderByteCode("standardVS")->GetBufferPointer()),
  //	My::DXRenderer::Instance().GetShaderByteCode("standardVS")->GetBufferSize()
  //};
  //opaquePsoDesc.PS =
  //{
  //	reinterpret_cast<BYTE*>(My::DXRenderer::Instance().GetShaderByteCode("opaquePS")->GetBufferPointer()),
  //	My::DXRenderer::Instance().GetShaderByteCode("opaquePS")->GetBufferSize()
  //};
  //opaquePsoDesc.RasterizerState = CD3DX12_RASTERIZER_DESC(D3D12_DEFAULT);
  //opaquePsoDesc.BlendState = CD3DX12_BLEND_DESC(D3D12_DEFAULT);
  //opaquePsoDesc.DepthStencilState = CD3DX12_DEPTH_STENCIL_DESC(D3D12_DEFAULT);
  //opaquePsoDesc.SampleMask = UINT_MAX;
  //opaquePsoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
  //opaquePsoDesc.NumRenderTargets = 1;
  //opaquePsoDesc.RTVFormats[0] = mBackBufferFormat;
  //opaquePsoDesc.SampleDesc.Count = m4xMsaaState ? 4 : 1;
  //opaquePsoDesc.SampleDesc.Quality = m4xMsaaState ? (m4xMsaaQuality - 1) : 0;
  //opaquePsoDesc.DSVFormat = mDepthStencilFormat;
  auto opaquePsoDesc = My::MyDX12::Desc::PSO::Basic(
      My::DXRenderer::Instance().GetRootSignature("default"),
      mInputLayout.data(), (UINT)mInputLayout.size(),
      My::DXRenderer::Instance().GetShaderByteCode("standardVS"),
      My::DXRenderer::Instance().GetShaderByteCode("opaquePS"),
      mBackBufferFormat, mDepthStencilFormat);
  //ThrowIfFailed(myDevice->CreateGraphicsPipelineState(&opaquePsoDesc, IID_PPV_ARGS(&mOpaquePSO)));
  My::DXRenderer::Instance().RegisterPSO("opaque", &opaquePsoDesc);
}

void DeferApp::BuildFrameResources() {
  for (int i = 0; i < gNumFrameResources; ++i) {
    auto fr = std::make_unique<My::MyDX12::FrameRsrcMngr>(mCurrentFence,
                                                          mFence.Get());

    Microsoft::WRL::ComPtr<ID3D12CommandAllocator> allocator;
    ThrowIfFailed(myDevice->CreateCommandAllocator(
        D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&allocator)));

    fr->RegisterResource("CommandAllocator", std::move(allocator));

    fr->RegisterResource("ArrayUploadBuffer<PassConstants>",
                         My::MyDX12::ArrayUploadBuffer<PassConstants>{
                             myDevice.raw.Get(), 1, true});

    fr->RegisterResource("ArrayUploadBuffer<MaterialConstants>",
                         My::MyDX12::ArrayUploadBuffer<MaterialConstants>{
                             myDevice.raw.Get(), mMaterials.size(), true});

    fr->RegisterResource("ArrayUploadBuffer<ObjectConstants>",
                         My::MyDX12::ArrayUploadBuffer<ObjectConstants>{
                             myDevice.raw.Get(), mAllRitems.size(), true});

    auto fgRsrcMngr = std::make_shared<My::MyDX12::FG::RsrcMngr>();
    fgRsrcMngr->Init(myGCmdList, myDevice);
    fr->RegisterResource("FrameGraphRsrcMngr", std::move(fgRsrcMngr));

    mFrameResources.emplace_back(std::move(fr));

    // We cannot update a cbuffer until the GPU is done processing the commands
    // that reference it.  So each frame needs their own cbuffers.
    // std::unique_ptr<My::MyDX12::ArrayUploadBuffer<FrameConstants>> FrameCB = nullptr;
    /*std::unique_ptr<My::MyDX12::ArrayUploadBuffer<PassConstants>> PassCB = nullptr;
		std::unique_ptr<My::MyDX12::ArrayUploadBuffer<MaterialConstants>> MaterialCB = nullptr;
		std::unique_ptr<My::MyDX12::ArrayUploadBuffer<ObjectConstants>> ObjectCB = nullptr;

        mFrameResources.push_back(std::make_unique<FrameResource>(myDevice.raw.Get(),
            1, (UINT)mAllRitems.size(), (UINT)mMaterials.size()));*/
  }
}

void DeferApp::BuildMaterials() {
  auto woodCrate = std::make_unique<Material>();
  woodCrate->Name = "woodCrate";
  woodCrate->MatCBIndex = 0;
  woodCrate->DiffuseSrvGpuHandle =
      My::DXRenderer::Instance().GetTextureSrvGpuHandle("woodCrateTex");
  woodCrate->DiffuseAlbedo = XMFLOAT4(1.0f, 1.0f, 1.0f, 1.0f);
  woodCrate->FresnelR0 = XMFLOAT3(0.05f, 0.05f, 0.05f);
  woodCrate->Roughness = 0.2f;

  mMaterials["woodCrate"] = std::move(woodCrate);
}

void DeferApp::BuildRenderItems() {
  auto boxRitem = std::make_unique<RenderItem>();
  boxRitem->ObjCBIndex = 0;
  boxRitem->Mat = mMaterials["woodCrate"].get();
  boxRitem->Geo = &My::DXRenderer::Instance().GetMeshGeometry("boxGeo");
  boxRitem->PrimitiveType = D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
  boxRitem->IndexCount = boxRitem->Geo->submeshGeometries["box"].IndexCount;
  boxRitem->StartIndexLocation =
      boxRitem->Geo->submeshGeometries["box"].StartIndexLocation;
  boxRitem->BaseVertexLocation =
      boxRitem->Geo->submeshGeometries["box"].BaseVertexLocation;
  mAllRitems.push_back(std::move(boxRitem));

  // All the render items are opaque.
  for (auto& e : mAllRitems)
    mOpaqueRitems.push_back(e.get());
}

void DeferApp::DrawRenderItems(ID3D12GraphicsCommandList* cmdList,
                               const std::vector<RenderItem*>& ritems) {
  UINT objCBByteSize =
      My::MyDX12::Util::CalcConstantBufferByteSize(sizeof(ObjectConstants));
  UINT matCBByteSize =
      My::MyDX12::Util::CalcConstantBufferByteSize(sizeof(MaterialConstants));

  auto objectCB =
      mCurrFrameRsrcMngr
          ->GetResource<My::MyDX12::ArrayUploadBuffer<ObjectConstants>>(
              "ArrayUploadBuffer<ObjectConstants>")
          .GetResource();
  auto matCB =
      mCurrFrameRsrcMngr
          ->GetResource<My::MyDX12::ArrayUploadBuffer<MaterialConstants>>(
              "ArrayUploadBuffer<MaterialConstants>")
          .GetResource();

  // For each render item...
  for (size_t i = 0; i < ritems.size(); ++i) {
    auto ri = ritems[i];

    cmdList->IASetVertexBuffers(0, 1, &ri->Geo->VertexBufferView());
    cmdList->IASetIndexBuffer(&ri->Geo->IndexBufferView());
    cmdList->IASetPrimitiveTopology(ri->PrimitiveType);

    /*CD3DX12_GPU_DESCRIPTOR_HANDLE tex(mSrvDescriptorHeap->GetGPUDescriptorHandleForHeapStart());*/
    /*CD3DX12_GPU_DESCRIPTOR_HANDLE tex(mSrvDescriptorHeap.GetGpuHandle());
		tex.Offset(ri->Mat->DiffuseSrvGpuHandle, mCbvSrvDescriptorSize);*/

    D3D12_GPU_VIRTUAL_ADDRESS objCBAddress =
        objectCB->GetGPUVirtualAddress() + ri->ObjCBIndex * objCBByteSize;
    D3D12_GPU_VIRTUAL_ADDRESS matCBAddress =
        matCB->GetGPUVirtualAddress() + ri->Mat->MatCBIndex * matCBByteSize;

    cmdList->SetGraphicsRootDescriptorTable(0, ri->Mat->DiffuseSrvGpuHandle);
    //cmdList->SetGraphicsRootShaderResourceView(0, mTextures["woodCrate"]->Resource->GetGPUVirtualAddress());
    cmdList->SetGraphicsRootConstantBufferView(1, objCBAddress);
    cmdList->SetGraphicsRootConstantBufferView(3, matCBAddress);

    cmdList->DrawIndexedInstanced(ri->IndexCount, 1, ri->StartIndexLocation,
                                  ri->BaseVertexLocation, 0);
  }
}
