# https://lldb.llvm.org/use/variable.html
# https://lldb.llvm.org/python_api.html
import lldb

def fff(member_list: lldb.SBTypeEnumMemberList, name: str) -> int:
	for member in member_list:
		if member.name == name:
			return member.signed
	raise ValueError('Value with name "{name}" not present in {member_list}')

def get_kind_name(valobj: lldb.SBValue) -> str:
	Kind_enum_members = valobj.target.FindFirstType('Kind').GetEnumMembers()
	kind = valobj.GetChildMemberWithName('kind').signed
	return Kind_enum_members[kind].name

def ExprNodeSummary(valobj: lldb.SBValue, internal_dict, options) -> str:
	# TODO: add the index in node_pool
	return get_kind_name(valobj)

class ExprNodeSyntheticChildrenProvider:
	def __init__(self, valobj: lldb.SBValue, internal_dict):
		self.kind = valobj.GetChildMemberWithName('kind')
		self.name = valobj.GetChildMemberWithName('name')
		# TODO: make them lldb.SBValue of ExprNode
		self.arg0 = valobj.GetChildMemberWithName('arg0')
		self.arg1 = valobj.GetChildMemberWithName('arg1')

		Kind_members = valobj.target.FindFirstType('Kind').GetEnumMembers()
		has_at_least_one_arg = (
			    self.kind.signed != fff(Kind_members, 'KIND_NULL')
			and self.kind.signed != fff(Kind_members, 'KIND_CONST')
			and self.kind.signed != fff(Kind_members, 'KIND_VAR')
		)
		self.childrens = ['kind', self.kind]
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
