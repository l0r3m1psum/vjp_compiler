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
		"then you can set the environment variable OMP_PREFIX=openmp14")

import torch

W1 = torch.ones(4,3, requires_grad=True)
W2 = torch.ones(4,4, requires_grad=True)

x = torch.ones(3)
y = torch.ones(4)

def l(x: torch.Tensor, y: torch.Tensor) -> torch.Tensor:
	y_hat = W2@torch.nn.functional.sigmoid(W1@x)
	return torch.nn.functional.mse_loss(y_hat, y)

l_compiled = torch.compile(l, fullgraph=True)
l_compiled(x, y).backward()