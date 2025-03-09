//***************************************************************************************
// DeferApp.cpp by Frank Luna (C) 2015 All Rights Reserved.
//***************************************************************************************

#include <MyDX12/UploadBuffer.h>

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
  // indices [NUM_DIR_LIGHTS+NUM_POINT_LIGHTS,
  // NUM_DIR_LIGHTS+NUM_POINT_LIGHT+NUM_SPOT_LIGHTS) are spot lights for a
  // maximum of MaxLights per object.
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

  // Dirty flag indicating the object data has changed and we need to update the
  // constant buffer. Because we have an object cbuffer for each FrameResource,
  // we have to apply the update to each FrameResource.  Thus, when we modify
  // obect data we should set NumFramesDirty = gNumFrameResources so that each
  // frame resource gets the update.
  int NumFramesDirty = gNumFrameResources;

  // Index into GPU constant buffer corresponding to the ObjectCB for this
  // render item.
  UINT ObjCBIndex = -1;

  Material* Mat = nullptr;
  My::DX12::MeshGeometry* Geo = nullptr;

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

  std::array<const CD3DX12_STATIC_SAMPLER_DESC, 6> GetStaticSamplers();

 private:
  std::vector<std::unique_ptr<My::DX12::FrameResource>> mFrameResources;
  My::DX12::FrameResource* mCurrFrameResource = nullptr;
  int mCurrFrameResourceIndex = 0;

  std::unordered_map<std::string, std::unique_ptr<Material>> mMaterials;

  std::vector<D3D12_INPUT_ELEMENT_DESC> mInputLayout;

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
  // My::DX12::FG::RsrcMngr fgRsrcMngr;
  My::DX12::FG::Executor fgExecutor;
  My::FG::Compiler fgCompiler;
  My::FG::FrameGraph fg;
};

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE prevInstance, PSTR cmdLine,
                   int showCmd) {
  // Enable run-time memory check for debug builds.
#if defined(DEBUG) | defined(_DEBUG)
  _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

  try {
    DeferApp theApp(hInstance);
    if (!theApp.Initialize()) return 0;

    int rst = theApp.Run();
    My::DXRenderer::Instance().Release();
    return rst;
  } catch (My::DX12::Util::Exception& e) {
    MessageBox(nullptr, e.ToString().c_str(), L"HR Failed", MB_OK);
    return 0;
  }
}

DeferApp::DeferApp(HINSTANCE hInstance) : D3DApp(hInstance) {}

DeferApp::~DeferApp() {
  if (!myDevice.IsNull()) FlushCommandQueue();
}

bool DeferApp::Initialize() {
  if (!D3DApp::Initialize()) return false;

  My::DXRenderer::Instance().Init(myDevice.raw.Get());

  My::DX12::DescriptorHeapMngr::Instance().Init(myDevice.raw.Get(), 1024, 1024,
                                                1024, 1024, 1024);

  // Reset the command list to prep for initialization commands.
  ThrowIfFailed(myGCmdList->Reset(mDirectCmdListAlloc.Get(), nullptr));

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

  // The window resized, so update the aspect ratio and recompute the projection
  // matrix.
  XMMATRIX P = XMMatrixPerspectiveFovLH(0.25f * MathHelper::Pi, AspectRatio(),
                                        1.0f, 1000.0f);
  XMStoreFloat4x4(&mProj, P);

  auto clearFGRsrcMngr = [](void* rsrcMngr) {
    reinterpret_cast<My::DX12::FG::RsrcMngr*>(rsrcMngr)->Clear();
  };
  for (auto& frsrc : mFrameResources)
    frsrc->DelayUpdateResource("FrameGraphRsrcMngr", clearFGRsrcMngr);
}

void DeferApp::Update(const GameTimer& gt) {
  OnKeyboardInput(gt);
  UpdateCamera(gt);

  // Cycle through the circular frame resource array.
  mCurrFrameResourceIndex = (mCurrFrameResourceIndex + 1) % gNumFrameResources;
  mCurrFrameResource = mFrameResources[mCurrFrameResourceIndex].get();

  // Has the GPU finished processing the commands of the current frame resource?
  // If not, wait until the GPU has completed commands up to this fence point.
  mCurrFrameResource->Wait();

  AnimateMaterials(gt);
  UpdateObjectCBs(gt);
  UpdateMaterialCBs(gt);
  UpdateMainPassCB(gt);
}

void DeferApp::Draw(const GameTimer& gt) {
  auto cmdListAlloc = mCurrFrameResource->GetResource<ID3D12CommandAllocator>(
      "CommandAllocator");

  // Reuse the memory associated with command recording.
  // We can only reset when the associated command lists have finished execution
  // on the GPU.
  ThrowIfFailed(cmdListAlloc->Reset());

  // A command list can be reset after it has been added to the command queue
  // via ExecuteCommandList. Reusing the command list reuses memory.
  ThrowIfFailed(myGCmdList->Reset(cmdListAlloc, nullptr));
  myGCmdList.SetDescriptorHeaps(My::DX12::DescriptorHeapMngr::Instance()
                                   .GetCSUGpuDH()
                                   ->GetDescriptorHeap());

  myGCmdList->RSSetViewports(1, &mScreenViewport);
  myGCmdList->RSSetScissorRects(1, &mScissorRect);

  fg.Clear();
  auto fgRsrcMngr = mCurrFrameResource->GetResource<My::DX12::FG::RsrcMngr>(
      "FrameGraphRsrcMngr");
  fgRsrcMngr->NewFrame();
  fgExecutor.NewFrame();
  ;

  auto gbuffer0 = fg.AddResourceNode("GBuffer0");
  auto gbuffer1 = fg.AddResourceNode("GBuffer1");
  auto gbuffer2 = fg.AddResourceNode("GBuffer2");
  auto backbuffer = fg.AddResourceNode("Back Buffer");
  auto depthstencil = fg.AddResourceNode("Depth Stencil");
  auto gbPass = fg.AddPassNode("GBuffer Pass", {},
                               {gbuffer0, gbuffer1, gbuffer2, depthstencil});
  auto debugPass = fg.AddPassNode("Debug", {gbuffer1}, {backbuffer});

  (*fgRsrcMngr)
      .RegisterTemporalRsrc(
          gbuffer0, My::DX12::FG::RsrcType::RT2D(DXGI_FORMAT_R32G32B32A32_FLOAT,
                                                 mClientWidth, mClientHeight,
                                                 Colors::Black))
      .RegisterTemporalRsrc(
          gbuffer1, My::DX12::FG::RsrcType::RT2D(DXGI_FORMAT_R32G32B32A32_FLOAT,
                                                 mClientWidth, mClientHeight,
                                                 Colors::Black))
      .RegisterTemporalRsrc(
          gbuffer2, My::DX12::FG::RsrcType::RT2D(DXGI_FORMAT_R32G32B32A32_FLOAT,
                                                 mClientWidth, mClientHeight,
                                                 Colors::Black))

      .RegisterRsrcTable(
          {{gbuffer0,
            My::DX12::Desc::SRV::Tex2D(DXGI_FORMAT_R32G32B32A32_FLOAT)},
           {gbuffer1,
            My::DX12::Desc::SRV::Tex2D(DXGI_FORMAT_R32G32B32A32_FLOAT)},
           {gbuffer2,
            My::DX12::Desc::SRV::Tex2D(DXGI_FORMAT_R32G32B32A32_FLOAT)}})

      .RegisterImportedRsrc(backbuffer,
                            {CurrentBackBuffer(), D3D12_RESOURCE_STATE_PRESENT})
      .RegisterImportedRsrc(depthstencil, {mDepthStencilBuffer.Get(),
                                           D3D12_RESOURCE_STATE_DEPTH_WRITE})

      .RegisterPassRsrcs(gbPass, gbuffer0, D3D12_RESOURCE_STATE_RENDER_TARGET,
                         My::DX12::FG::RsrcImplDesc_RTV_Null{})
      .RegisterPassRsrcs(gbPass, gbuffer1, D3D12_RESOURCE_STATE_RENDER_TARGET,
                         My::DX12::FG::RsrcImplDesc_RTV_Null{})
      .RegisterPassRsrcs(gbPass, gbuffer2, D3D12_RESOURCE_STATE_RENDER_TARGET,
                         My::DX12::FG::RsrcImplDesc_RTV_Null{})
      .RegisterPassRsrcs(gbPass, depthstencil, D3D12_RESOURCE_STATE_DEPTH_WRITE,
                         My::DX12::Desc::DSV::Basic(mDepthStencilFormat))

      .RegisterPassRsrcs(
          debugPass, gbuffer1, D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE,
          My::DX12::Desc::SRV::Tex2D(DXGI_FORMAT_R32G32B32A32_FLOAT))

      .RegisterPassRsrcs(debugPass, backbuffer,
                         D3D12_RESOURCE_STATE_RENDER_TARGET,
                         My::DX12::FG::RsrcImplDesc_RTV_Null{});

  fgExecutor.RegisterPassFunc(
      gbPass, [&](const My::DX12::FG::PassRsrcs& rsrcs) {
        myGCmdList->SetPipelineState(
            My::DXRenderer::Instance().GetPSO("geometry"));
        auto gb0 = rsrcs.find(gbuffer0)->second;
        auto gb1 = rsrcs.find(gbuffer1)->second;
        auto gb2 = rsrcs.find(gbuffer2)->second;
        auto ds = rsrcs.find(depthstencil)->second;

        // Clear the render texture and depth buffer.
        myGCmdList.ClearRenderTargetView(gb0.cpuHandle, Colors::Black);
        myGCmdList.ClearRenderTargetView(gb1.cpuHandle, Colors::Black);
        myGCmdList.ClearRenderTargetView(gb2.cpuHandle, Colors::Black);
        myGCmdList.ClearDepthStencilView(ds.cpuHandle);

        // Specify the buffers we are going to render to.
        std::array rts{gb0.cpuHandle, gb1.cpuHandle, gb2.cpuHandle};
        myGCmdList->OMSetRenderTargets(rts.size(), rts.data(), false,
                                      &ds.cpuHandle);

        myGCmdList->SetGraphicsRootSignature(
            My::DXRenderer::Instance().GetRootSignature("default"));

        auto passCB =
            mCurrFrameResource
                ->GetResource<My::DX12::ArrayUploadBuffer<PassConstants>>(
                    "gbPass constants")
                ->GetResource();
        myGCmdList->SetGraphicsRootConstantBufferView(
            2, passCB->GetGPUVirtualAddress());

        DrawRenderItems(myGCmdList.raw.Get(), mOpaqueRitems);
      });

  fgExecutor.RegisterPassFunc(
      debugPass, [&](const My::DX12::FG::PassRsrcs& rsrcs) {
        myGCmdList->SetPipelineState(
            My::DXRenderer::Instance().GetPSO("screen"));
        auto img = rsrcs.find(gbuffer1)->second;
        auto bb = rsrcs.find(backbuffer)->second;

        // myGCmdList->CopyResource(bb.resource, rt.resource);

        // Clear the render texture and depth buffer.
        myGCmdList.ClearRenderTargetView(bb.cpuHandle, Colors::LightSteelBlue);

        // Specify the buffers we are going to render to.
        // myGCmdList.OMSetRenderTarget(bb.cpuHandle, ds.cpuHandle);
        myGCmdList->OMSetRenderTargets(1, &bb.cpuHandle, false, nullptr);

        myGCmdList->SetGraphicsRootSignature(
            My::DXRenderer::Instance().GetRootSignature("screen"));

        myGCmdList->SetGraphicsRootDescriptorTable(0, img.gpuHandle);

        myGCmdList->IASetVertexBuffers(0, 0, nullptr);
        myGCmdList->IASetIndexBuffer(nullptr);
        myGCmdList->IASetPrimitiveTopology(D3D_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
        myGCmdList->DrawInstanced(6, 1, 0, 0);
      });

  static bool flag{false};
  if (!flag) {
    OutputDebugStringA(fg.ToGraphvizGraph().Dump().c_str());
    flag = true;
  }

  auto [success, crst] = fgCompiler.Compile(fg);
  fgExecutor.Execute(crst, *fgRsrcMngr);

  // Done recording commands.
  ThrowIfFailed(myGCmdList->Close());

  // Add the command list to the queue for execution.
  myCmdQueue.Execute(myGCmdList.raw.Get());

  // Swap the back and front buffers
  ThrowIfFailed(mSwapChain->Present(0, 0));
  mCurrBackBuffer = (mCurrBackBuffer + 1) % SwapChainBufferCount;

  mCurrFrameResource->Signal(myCmdQueue.raw.Get(), ++mCurrentFence);
}

void DeferApp::OnMouseDown(WPARAM btnState, int x, int y) {
  mLastMousePos.x = x;
  mLastMousePos.y = y;

  SetCapture(mhMainWnd);
}

void DeferApp::OnMouseUp(WPARAM btnState, int x, int y) { ReleaseCapture(); }

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
  auto currObjectCB =
      mCurrFrameResource
          ->GetResource<My::DX12::ArrayUploadBuffer<ObjectConstants>>(
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

      currObjectCB->Set(e->ObjCBIndex, objConstants);

      // Next FrameResource need to be updated too.
      e->NumFramesDirty--;
    }
  }
}

void DeferApp::UpdateMaterialCBs(const GameTimer& gt) {
  auto currMaterialCB =
      mCurrFrameResource
          ->GetResource<My::DX12::ArrayUploadBuffer<MaterialConstants>>(
              "ArrayUploadBuffer<MaterialConstants>");
  for (auto& e : mMaterials) {
    // Only update the cbuffer data if the constants have changed.  If the
    // cbuffer data changes, it needs to be updated for each FrameResource.
    Material* mat = e.second.get();
    if (mat->NumFramesDirty > 0) {
      XMMATRIX matTransform = XMLoadFloat4x4(&mat->MatTransform);

      MaterialConstants matConstants;
      matConstants.DiffuseAlbedo = mat->DiffuseAlbedo;
      matConstants.FresnelR0 = mat->FresnelR0;
      matConstants.Roughness = mat->Roughness;
      XMStoreFloat4x4(&matConstants.MatTransform,
                      XMMatrixTranspose(matTransform));

      currMaterialCB->Set(mat->MatCBIndex, matConstants);

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

  auto currPassCB =
      mCurrFrameResource
          ->GetResource<My::DX12::ArrayUploadBuffer<PassConstants>>(
              "gbPass constants");
  currPassCB->Set(0, mMainPassCB);
}

void DeferApp::LoadTextures() {
  My::DXRenderer::Instance().RegisterDDSTextureFromFile(
      My::DXRenderer::Instance().GetUpload(), "woodCrateTex",
      L"../data/textures/WoodCrate01.dds");
}

void DeferApp::BuildRootSignature() {
  {  // default
    CD3DX12_DESCRIPTOR_RANGE texTable;
    texTable.Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0);

    // Root parameter can be a table, root descriptor or root constants.
    CD3DX12_ROOT_PARAMETER slotRootParameter[4];

    // Perfomance TIP: Order from most frequent to least frequent.
    slotRootParameter[0].InitAsDescriptorTable(1, &texTable,
                                               D3D12_SHADER_VISIBILITY_PIXEL);
    slotRootParameter[1].InitAsConstantBufferView(0);
    slotRootParameter[2].InitAsConstantBufferView(1);
    slotRootParameter[3].InitAsConstantBufferView(2);

    auto staticSamplers = GetStaticSamplers();

    // A root signature is an array of root parameters.
    CD3DX12_ROOT_SIGNATURE_DESC rootSigDesc(
        4, slotRootParameter, (UINT)staticSamplers.size(),
        staticSamplers.data(),
        D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

    My::DXRenderer::Instance().RegisterRootSignature("default", &rootSigDesc);
  }

  {  // screen
    CD3DX12_DESCRIPTOR_RANGE texTable;
    texTable.Init(D3D12_DESCRIPTOR_RANGE_TYPE_SRV, 1, 0);

    // Root parameter can be a table, root descriptor or root constants.
    CD3DX12_ROOT_PARAMETER slotRootParameter[1];

    // Perfomance TIP: Order from most frequent to least frequent.
    slotRootParameter[0].InitAsDescriptorTable(1, &texTable,
                                               D3D12_SHADER_VISIBILITY_PIXEL);

    auto staticSamplers = GetStaticSamplers();

    // A root signature is an array of root parameters.
    CD3DX12_ROOT_SIGNATURE_DESC rootSigDesc(
        1, slotRootParameter, (UINT)staticSamplers.size(),
        staticSamplers.data(),
        D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

    My::DXRenderer::Instance().RegisterRootSignature("screen", &rootSigDesc);
  }
}

void DeferApp::BuildDescriptorHeaps() {}

void DeferApp::BuildShadersAndInputLayout() {
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "standardVS", L"..\\data\\shaders\\01_defer\\Default.hlsl", nullptr, "VS",
      "vs_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "opaquePS", L"..\\data\\shaders\\01_defer\\Default.hlsl", nullptr, "PS",
      "ps_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "screenVS", L"..\\data\\shaders\\01_defer\\Screen.hlsl", nullptr, "VS",
      "vs_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "screenPS", L"..\\data\\shaders\\01_defer\\Screen.hlsl", nullptr, "PS",
      "ps_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "geometryVS", L"..\\data\\shaders\\01_defer\\Geometry.hlsl", nullptr,
      "VS", "vs_5_0");
  My::DXRenderer::Instance().RegisterShaderByteCode(
      "geometryPS", L"..\\data\\shaders\\01_defer\\Geometry.hlsl", nullptr,
      "PS", "ps_5_0");

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

  My::DX12::SubmeshGeometry boxSubmesh;
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

  My::DXRenderer::Instance()
      .RegisterStaticMeshGeometry(
          My::DXRenderer::Instance().GetUpload(), "boxGeo", vertices.data(),
          (UINT)vertices.size(), sizeof(Vertex), indices.data(),
          (UINT)indices.size(), DXGI_FORMAT_R16_UINT)
      .submeshGeometries["box"] = boxSubmesh;
}

void DeferApp::BuildPSOs() {
  auto opaquePsoDesc = My::DX12::Desc::PSO::Basic(
      My::DXRenderer::Instance().GetRootSignature("default"),
      mInputLayout.data(), (UINT)mInputLayout.size(),
      My::DXRenderer::Instance().GetShaderByteCode("standardVS"),
      My::DXRenderer::Instance().GetShaderByteCode("opaquePS"),
      mBackBufferFormat, mDepthStencilFormat);
  My::DXRenderer::Instance().RegisterPSO("opaque", &opaquePsoDesc);

  auto screenPsoDesc = My::DX12::Desc::PSO::Basic(
      My::DXRenderer::Instance().GetRootSignature("screen"), nullptr, 0,
      My::DXRenderer::Instance().GetShaderByteCode("screenVS"),
      My::DXRenderer::Instance().GetShaderByteCode("screenPS"),
      mBackBufferFormat, DXGI_FORMAT_UNKNOWN);
  My::DXRenderer::Instance().RegisterPSO("screen", &screenPsoDesc);

  auto geometryPsoDesc = My::DX12::Desc::PSO::MRT(
      My::DXRenderer::Instance().GetRootSignature("default"),
      mInputLayout.data(), (UINT)mInputLayout.size(),
      My::DXRenderer::Instance().GetShaderByteCode("geometryVS"),
      My::DXRenderer::Instance().GetShaderByteCode("geometryPS"), 3,
      DXGI_FORMAT_R32G32B32A32_FLOAT, mDepthStencilFormat);
  My::DXRenderer::Instance().RegisterPSO("geometry", &geometryPsoDesc);
}

void DeferApp::BuildFrameResources() {
  for (int i = 0; i < gNumFrameResources; ++i) {
    auto fr = std::make_unique<My::DX12::FrameResource>(mFence.Get());

    ID3D12CommandAllocator* allocator;
    ThrowIfFailed(myDevice->CreateCommandAllocator(
        D3D12_COMMAND_LIST_TYPE_DIRECT, IID_PPV_ARGS(&allocator)));

    fr->RegisterResource("CommandAllocator", allocator, [](void* allocator) {
      reinterpret_cast<ID3D12CommandAllocator*>(allocator)->Release();
    });

    fr->RegisterResource("gbPass constants",
                         new My::DX12::ArrayUploadBuffer<PassConstants>{
                             myDevice.raw.Get(), 1, true});

    fr->RegisterResource("ArrayUploadBuffer<MaterialConstants>",
                         new My::DX12::ArrayUploadBuffer<MaterialConstants>{
                             myDevice.raw.Get(), mMaterials.size(), true});

    fr->RegisterResource("ArrayUploadBuffer<ObjectConstants>",
                         new My::DX12::ArrayUploadBuffer<ObjectConstants>{
                             myDevice.raw.Get(), mAllRitems.size(), true});

    auto fgRsrcMngr = new My::DX12::FG::RsrcMngr;
    fgRsrcMngr->Init(myGCmdList, myDevice);
    fr->RegisterResource("FrameGraphRsrcMngr", fgRsrcMngr);

    mFrameResources.emplace_back(std::move(fr));
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
  for (auto& e : mAllRitems) mOpaqueRitems.push_back(e.get());
}

void DeferApp::DrawRenderItems(ID3D12GraphicsCommandList* cmdList,
                               const std::vector<RenderItem*>& ritems) {
  UINT objCBByteSize =
      My::DX12::Util::CalcConstantBufferByteSize(sizeof(ObjectConstants));
  UINT matCBByteSize =
      My::DX12::Util::CalcConstantBufferByteSize(sizeof(MaterialConstants));

  auto objectCB =
      mCurrFrameResource
          ->GetResource<My::DX12::ArrayUploadBuffer<ObjectConstants>>(
              "ArrayUploadBuffer<ObjectConstants>")
          ->GetResource();
  auto matCB =
      mCurrFrameResource
          ->GetResource<My::DX12::ArrayUploadBuffer<MaterialConstants>>(
              "ArrayUploadBuffer<MaterialConstants>")
          ->GetResource();

  // For each render item...
  for (size_t i = 0; i < ritems.size(); ++i) {
    auto ri = ritems[i];

    cmdList->IASetVertexBuffers(0, 1, &ri->Geo->VertexBufferView());
    cmdList->IASetIndexBuffer(&ri->Geo->IndexBufferView());
    cmdList->IASetPrimitiveTopology(ri->PrimitiveType);

    D3D12_GPU_VIRTUAL_ADDRESS objCBAddress =
        objectCB->GetGPUVirtualAddress() + ri->ObjCBIndex * objCBByteSize;
    D3D12_GPU_VIRTUAL_ADDRESS matCBAddress =
        matCB->GetGPUVirtualAddress() + ri->Mat->MatCBIndex * matCBByteSize;

    cmdList->SetGraphicsRootDescriptorTable(0, ri->Mat->DiffuseSrvGpuHandle);
    cmdList->SetGraphicsRootConstantBufferView(1, objCBAddress);
    cmdList->SetGraphicsRootConstantBufferView(3, matCBAddress);

    cmdList->DrawIndexedInstanced(ri->IndexCount, 1, ri->StartIndexLocation,
                                  ri->BaseVertexLocation, 0);
  }
}

std::array<const CD3DX12_STATIC_SAMPLER_DESC, 6> DeferApp::GetStaticSamplers() {
  // Applications usually only need a handful of samplers.  So just define them
  // all up front and keep them available as part of the root signature.

  const CD3DX12_STATIC_SAMPLER_DESC pointWrap(
      0,                                 // shaderRegister
      D3D12_FILTER_MIN_MAG_MIP_POINT,    // filter
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,   // addressU
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,   // addressV
      D3D12_TEXTURE_ADDRESS_MODE_WRAP);  // addressW

  const CD3DX12_STATIC_SAMPLER_DESC pointClamp(
      1,                                  // shaderRegister
      D3D12_FILTER_MIN_MAG_MIP_POINT,     // filter
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,   // addressU
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,   // addressV
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP);  // addressW

  const CD3DX12_STATIC_SAMPLER_DESC linearWrap(
      2,                                 // shaderRegister
      D3D12_FILTER_MIN_MAG_MIP_LINEAR,   // filter
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,   // addressU
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,   // addressV
      D3D12_TEXTURE_ADDRESS_MODE_WRAP);  // addressW

  const CD3DX12_STATIC_SAMPLER_DESC linearClamp(
      3,                                  // shaderRegister
      D3D12_FILTER_MIN_MAG_MIP_LINEAR,    // filter
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,   // addressU
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,   // addressV
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP);  // addressW

  const CD3DX12_STATIC_SAMPLER_DESC anisotropicWrap(
      4,                                // shaderRegister
      D3D12_FILTER_ANISOTROPIC,         // filter
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,  // addressU
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,  // addressV
      D3D12_TEXTURE_ADDRESS_MODE_WRAP,  // addressW
      0.0f,                             // mipLODBias
      8);                               // maxAnisotropy

  const CD3DX12_STATIC_SAMPLER_DESC anisotropicClamp(
      5,                                 // shaderRegister
      D3D12_FILTER_ANISOTROPIC,          // filter
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,  // addressU
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,  // addressV
      D3D12_TEXTURE_ADDRESS_MODE_CLAMP,  // addressW
      0.0f,                              // mipLODBias
      8);                                // maxAnisotropy

  return {pointWrap,   pointClamp,      linearWrap,
          linearClamp, anisotropicWrap, anisotropicClamp};
}
