#if !defined(backend_h)
#define backend_h

#include "ast.h"

// TODO: Doc
extern LLVMModuleRef backend_compile(AST*);

#endif
