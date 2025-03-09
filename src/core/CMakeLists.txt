ADD_TARGET(
        MODE STATIC
        SOURCE
        "${PROJECT_SOURCE_DIR}/include/MyDXRenderer"
        INC include
        LIB
        My::MyFG_DX12
)