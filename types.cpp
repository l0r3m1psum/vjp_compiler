// https://anadoxin.org/blog/buggy-lldb-api-cpp/
// https://github.com/llvm-mirror/lldb/blob/master/examples/plugins/commands/fooplugin.cpp
// https://lldb.llvm.org/cpp_reference/index.html
// https://lldb.llvm.org/cpp_reference/namespacelldb.html

#include <lldb/API/SBCommandInterpreter.h>
#include <lldb/API/SBCommandReturnObject.h>
#include <lldb/API/SBDebugger.h>
#include <lldb/API/SBError.h>
#include <lldb/API/SBProcess.h>
#include <lldb/API/SBStream.h>
#include <lldb/API/SBTypeCategory.h>
#include <lldb/API/SBTypeEnumMember.h>
#include <lldb/API/SBTypeNameSpecifier.h>
#include <lldb/API/SBTypeSummary.h>
#include <lldb/API/SBTypeSynthetic.h>

namespace lldb {
bool PluginInitialize(lldb::SBDebugger debugger);
}

class ChildCommand : public lldb::SBCommandPluginInterface {
public:
  virtual bool DoExecute(lldb::SBDebugger debugger, char **command,
                         lldb::SBCommandReturnObject &result) {
    if (command) {
      const char *arg = *command;
      while (arg) {
        result.Printf("%s\n", arg);
        arg = *(++command);
      }
      return true;
    }
    return false;
  }
};

// TODO: print the expression.
bool my_format_callback(lldb::SBValue value, lldb::SBTypeSummaryOptions options, lldb::SBStream &stream) {
  lldb::SBTarget target = value.GetTarget();
  {
    lldb::SBType Kind_enum = target.FindFirstType("Kind");
    lldb::SBTypeEnumMemberList kind_members = Kind_enum.GetEnumMembers();
    uint64_t kind_value = value.GetChildMemberWithName("kind").GetValueAsUnsigned();
    const char *name = kind_members.GetTypeEnumMemberAtIndex(kind_value).GetName();
    stream.Printf("%s", name);
  }
  {
    lldb::SBValue node_pool_value = target.FindFirstGlobalVariable("node_pool");
    lldb::SBProcess process = target.GetProcess();
    lldb::SBError error;
#if 0
    process.ReadMemory(
      node_pool_value.GetValueAsAddress(),
      node_pool,
      node_pool_value.GetByteSize(),
      error
    );
#endif
  }
  return true;
}

const char *i_hate_lldb =
R"XXX(
# https://lldb.llvm.org/use/variable.html
# https://lldb.llvm.org/python_api.html

def __init__(self, valobj: lldb.SBValue, internal_dict):

  def fff(member_list: lldb.SBTypeEnumMemberList, name: str) -> int:
    for member in member_list:
      if member.name == name:
        return member.signed
    raise ValueError('Value with name "{name}" not present in {member_list}')

  Kind_type = valobj.target.FindFirstType('Kind')

  self.kind = valobj.GetChildMemberWithName('kind')
  self.name = valobj.GetChildMemberWithName('name')
  # TODO: make them lldb.SBValue of ExprNode
  self.arg0 = valobj.GetChildMemberWithName('arg0')
  self.arg1 = valobj.GetChildMemberWithName('arg1')

  Kind_members = Kind_type.GetEnumMembers()
  has_at_least_one_arg = (
        self.kind.signed != fff(Kind_members, 'KIND_NULL')
    and self.kind.signed != fff(Kind_members, 'KIND_CONST')
    and self.kind.signed != fff(Kind_members, 'KIND_VAR')
  )
  self.childrens = ['kind', self.kind.CreateValueFromData('kind', self.kind.GetData(), Kind_type)]
  # self.childrens = ['kind', self.kind]
  if has_at_least_one_arg:
    self.childrens.extend(('name', self.name))
    if self.kind.signed != fff(Kind_members, 'KIND_DIFF'): self.childrens.extend(('arg0', self.arg0))
    if self.kind.signed != fff(Kind_members, 'KIND_TRANS'): self.childrens.extend(('arg1', self.arg1))

def num_children(self, max_children: int) -> int:
  assert len(self.childrens)%2 == 0
  return len(self.childrens)//2
def get_child_index(self, name: str) -> int:
  assert len(self.childrens)%2 == 0
  return self.childrens.index(name)//2
def get_child_at_index(self, index: int) -> lldb.SBValue:
  assert len(self.childrens)%2 == 0
  return self.childrens[index*2 + 1]
)XXX";

bool lldb::PluginInitialize(lldb::SBDebugger debugger) {
  lldb::SBCommandInterpreter interpreter = debugger.GetCommandInterpreter();
  lldb::SBCommand foo = interpreter.AddMultiwordCommand("foo", NULL);
  foo.AddCommand("child", new ChildCommand(), "a child of foo");

  // SyntheticChildrenFrontEnd
  lldb::SBTypeNameSpecifier type_name = lldb::SBTypeNameSpecifier("ExprNode");
  lldb::SBTypeSummary type_summary = lldb::SBTypeSummary::CreateWithCallback(my_format_callback);
  // FLAGS_ENUM(TypeOptions)
  lldb::SBTypeSynthetic type_synthetic = lldb::SBTypeSynthetic::CreateWithScriptCode(i_hate_lldb);
  // type_synthetic.SetClassName("ExprNodeSyntheticChildrenProvider");

  // lldb::SBTarget target = debugger.GetSelectedTarget();
  lldb::SBTypeCategory category = debugger.GetDefaultCategory();
  category.AddTypeSummary(type_name, type_summary);
  category.AddTypeSynthetic(type_name, type_synthetic);
  return true;
}