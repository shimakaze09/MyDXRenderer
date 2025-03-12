MACRO(INIT_MYCMAKE)
    INCLUDE(FETCHCONTENT)
    SET(MyCMake_VERSION 0.6.1)
    MESSAGE(STATUS "Looking for: MyCMake ${MyCMake_VERSION}")
    FIND_PACKAGE(MyCMake ${MyCMake_VERSION} QUIET)
    IF (NOT MyCMake_FOUND)
        SET(ADDRESS "https://github.com/shimakaze09/MyCMake.git")
        MESSAGE(STATUS "MyCMake ${MyCMake_VERSION} not found\n")
        MESSAGE(STATUS "Fetching from ${ADDRESS} with tag ${MyCMake_VERSION}")
        FETCHCONTENT_DECLARE(
                MyCMake
                GIT_REPOSITORY ${ADDRESS}
                GIT_TAG ${MyCMake_VERSION}
        )
        FETCHCONTENT_MAKEAVAILABLE(MyCMake)
        MESSAGE(STATUS "MyCMake ${MyCMake_VERSION} built")
    ENDIF ()
ENDMACRO()