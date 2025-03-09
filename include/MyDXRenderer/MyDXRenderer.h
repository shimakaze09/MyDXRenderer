//
// Created by Admin on 9/03/2025.
//

#pragma once

#include <d3d12.h>

#include <string>

namespace DirectX {
class ResourceUploadBatch;
}

namespace My {
class DXRenderer {
 public:
  static DXRenderer& Instance() noexcept {
    static DXRenderer instance;
    return instance;
  }

  DirectX::ResourceUploadBatch& GetUpload() const;

  DXRenderer& Init(ID3D12Device* device);
  void Release();

  DXRenderer& RegisterDDSTextureFromFile(DirectX::ResourceUploadBatch& upload,
                                         std::string name,
                                         std::wstring filename);

  D3D12_GPU_DESCRIPTOR_HANDLE GetTextureGpuHandle(
      const std::string& name) const;

 private:
  struct Impl;
  Impl* pImpl;

  DXRenderer();
  ~DXRenderer();
};
}  // namespace My
