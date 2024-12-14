# As always there is a lot of clowning from Python, PyTorch and Apple combined.
import os, sys

if ' ' in __file__:
	raise Exception("Torch Inductor requires paths that do not include spaces "
		"because it passes them to the compiler unquoted")
if sys.platform == 'darwin' and 'OMP_PREFIX' not in os.environ:
	raise Exception("Apple Clang does not include OpenMP support. To solve this"
		" problem you can follow the instructions on "
		"https://mac.r-project.org/openmp/ to download a pre-compiled version "
		"and unpack it to a local directory with the following commands:\n"
		"curl -O https://mac.r-project.org/openmp/openmp-14.0.6-darwin20-Release.tar.gz\n"
		"tar fvxz openmp-14.0.6-darwin20-Release.tar.gz -C openmp14\n"
		"then you can set the environment variable "
		"OMP_PREFIX=openmp14/usr/local")

# TODO: use torch.fx more directly to skip the Dynamo step and use
# torch._inductor.compile_fx
# https://pytorch.org/docs/stable/fx.html#torch.fx.Graph
# https://github.com/pytorch/pytorch/blob/main/torch/_inductor/compile_fx.py

# TODO: I have to inject my alogrithm in a function like this one:
# https://github.com/pytorch/pytorch/blob/91bf2e16debdc41f5dde2bb5cc8e4f39f8955d4e/torch/_inductor/fx_passes/group_batch_fusion.py#L1294
# But also for other kind of fusions and only for the backward pass.

import torch
import torch._inductor.config
torch._inductor.config.debug = True

W1 = torch.ones(4,3, requires_grad=True)
W2 = torch.ones(4,4, requires_grad=True)

x = torch.ones(3)
y = torch.ones(4)

def l(x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
	h = torch.nn.functional.sigmoid(W1@x)
	y_hat = W2@h
	return torch.nn.functional.mse_loss(y_hat, y)

l_compiled = torch.compile(l, fullgraph=True, options={'trace.enabled':True,
	'trace.graph_diagram':True})
l_compiled(x, y).backward()