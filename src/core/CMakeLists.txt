ADD_TARGET(
        MODE STATIC
        SOURCE
        "${PROJECT_SOURCE_DIR}/include/MyDXRenderer"
        INC
        "${PROJECT_SOURCE_DIR}/include"
        LIB
        My::MyDX12_core
)