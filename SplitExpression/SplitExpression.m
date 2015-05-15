(* ::Package:: *)

(* ::Title:: *)
(*SplitExpression*)


(* ::Text:: *)
(*:Author:*)
(*Joseph Elliston*)


(* ::Text:: *)
(*:Summary:*)
(*The SplitExpression package extends the xTensor package to allow for simple decomposition of tensorial expressions by splitting index summations into parts and automatically defining new tensors as required. Another function is provided to replace these tensors with user-defined rules, if desired. We also provide the facility for single index components to be rewritten as parameters, avoiding unneccesary index proliferation. The motivation behing this package is to provide low-level functionality for operations such as the 3+1 split in General Relativity.*)


(* ::Text:: *)
(*:Context: *)
(*xAct`SplitExpression`*)


(* ::Text:: *)
(*:History:*)
(*Current version: v1.0.2 (2015.05.15)*)
(*See SplitExpression.history for incremental updates*)
(**)


(* ::Section::Closed:: *)
(*Preliminaries*)


(* ::Subsection::Closed:: *)
(*GPL*)


(* ::Text:: *)
(*This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version.*)
(**)
(*This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.*)
(**)
(*You should have received a copy of the GNU General Public License along with xAct; if not, write to the Free Software Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA. *)


(* ::Subsection::Closed:: *)
(*Version numbers and package dependencies*)


(* ::Text:: *)
(*Package version number:*)


xAct`SplitExpression`$Version={"1.0.2",{2015,5,15}};


(* ::Text:: *)
(*Expected version number for xTensor:*)


xAct`SplitExpression`$xTensorVersionExpected={"1.0.5",{2013,1,30}};


(* ::Text:: *)
(*In case the package is loaded multiple times, wipe the memory of all package symbols apart from the version numbers defined above.*)


With[
	{xAct`SplitExpression`Private`SplitExpressionSymbols=
		DeleteCases[
			Join[Names["xAct`SplitExpression`*"],Names["xAct`SplitExpression`Private`*"]],
			"$Version"|"xAct`SplitExpression`$Version"|"$xTensorVersionExpected"|"xAct`SplitExpression`$xTensorVersionExpected"
		]
	},
	Unprotect/@xAct`SplitExpression`Private`SplitExpressionSymbols;
	Clear/@xAct`SplitExpression`Private`SplitExpressionSymbols;
];


(* ::Text:: *)
(*Set this package to be the last package to load*)


If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,
	xAct`xCore`Private`$LastPackage="xAct`SplitExpression`";
];


(* ::Text:: *)
(*Begin the package and load dependencies*)


BeginPackage["xAct`SplitExpression`",{"xAct`xCore`","xAct`xPerm`","xAct`xTensor`"}];


(* ::Text:: *)
(*Check version of xTensor:*)


If[
	Not@OrderedQ@Map[Last,{$xTensorVersionExpected,xAct`xTensor`$Version}],
	Message[General::versions,"xTensor",xAct`xTensor`$Version,$xTensorVersionExpected];
	Abort[];
];


(* ::Subsection::Closed:: *)
(*Output message after loading the package*)


Print[xAct`xCore`Private`bars];
Print["Package xAct`SplitExpression version ",$Version[[1]],", ",$Version[[2]]];
Print["CopyRight (C) 2014-2015, Joseph Elliston, under the General Public License."];


(* ::Text:: *)
(*We specify the context xAct`SplitExpression` to avoid overriding the Disclaimer of xCore, xPerm and xTensor. However we need to turn off the message General:shdw temporarily:*)


Off[General::shdw];
xAct`SplitExpression`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."];
On[General::shdw];


(* ::Text:: *)
(*If xAct`SplitExpression` is the last package read, then print the short GPL disclaimer:*)


If[xAct`xCore`Private`$LastPackage==="xAct`SplitExpression`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];
Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars];
];


(* ::Subsection::Closed:: *)
(*Reduce and configure output*)


$DefInfoQ = False;
$UndefInfoQ = False;


(* ::Text:: *)
(*Make L-indices blue:*)


Unprotect[IndexForm];
IndexForm[LI[ind_]]:=xAct`xCore`ColorString[ToString[ind], RGBColor[0, 0, 1]];
Protect[IndexForm];


(* ::Subsection::Closed:: *)
(*Detailed package description*)


$SplitExpressionDescription = "
   This package allows the user to perform arbitrary index splitting operations on tensorial expressions. One use of this package is to expand tensorial expressions into component form, but the package also allows for the range of values that an index takes to be split into an arbitrary combination of smaller ranges. 
    
   When the indices on a tensor are split then the tensor components are effectively split into block form. This package automatically defines each of the block components as a new tensor, and for clarity we refer to these new tensors as \[OpenCurlyQuote]split tensors\[CloseCurlyQuote]. 
    
   Dummy/contracted indices are split into separate summations over the new index ranges. Free indices behave differently, and splitting these yields multiple output terms corresponding to each possible choice for splitting each free indice. 
    
   It is commonplace to separate single values from an index range so that they no longer appear in any index summations. For example, when performing the '3+1' split in General Relatively where one splits 4-dimensional spacetime indices into 3-dimensional spatial indices and 1-dimensional time. When performing such an index splitting, this package mimmics the way that the calculation would be done by hand and the split-tensors associated to single index values are defined to have a lower rank and as such they are not furnished with pointless indices that can only take one possible value. Note that the resulting split-tensors will in general have *parametric* dependence on all of the previous variables and this is accounted for automatically. This parametric dependence is a non-trivial coding exercise, and a key motivation for the creation of this package.
    
   The tensorial indices that will be decomposed are assumed to live on a given tangent bundle. In xAct, these indices can be abstract (A) indices, basis (B) indices or individual component (C) indices. C-indices cannot be split since they are already written in terms of individual components. B-indices can arise whenever a basis has been defined, and A-indices are understood to be defined with respect to some fiducial basis. B- and C-indices are principally used in the xCoba package. In this SplitExpression package, we purposefully avoid using such basis or component indices and work only with A-indices. This choice is partially pragmatic, allowing us to use the greater functionality of xTensor in manipulating A-indices to yield considerably simpler code. The only restriction imposed by choosing to only work with A-indices is the scenario with both A- and B-indices simultaneously present in the same VBundle, and this is a potentially confusing scenario in any case.
    
   The indices before and after the splitting must be defined first. Indices belong to a VBundle. If we also require a metric to raise and lower any of these indices, then xTensor demands that the VBundle is tangent to a manifold, and is known as a TangentVBundle. To avoid the complication of dealing with some VBundles being TangentVBundles and others not, we stipulate that all \[IGrave]ndices are defined on TangentVBundles. Since a TangentVBundle is created automatically when a manifold is defined, instead of working with VBundles we can work with manifolds; one must define a manifold for the original space (along with its indices) and manifolds for all of the subspaces (along with their indices). Parameters, as discussed, will not have indices and so no manifolds need be defined for these. Returning to the '3+1' example, the discussion above means that at the start of the code one needs to define a spacetime manifold and a spacial manifold, with indices on each manifold, but no time manifold is required.
    
   For orientation putposes, let us now discuss the likely way in which the SplitExpression package will be used and summarise what the main functions do:
    
   1) One will begin by defining a manifold and some tensors to obtain the tensorial expression that one wants to split.
   2) Next one will need to define manifolds, indices and parameters that will appear in the expression after it is split.
   3) The splitting is defined using the function DefSplitting to define a splitting object. This function doesn't acutally do any splitting itself, but it stores the details of the splitting and associates these details to a name, e.g. 'MySplitting'. This allows the user to use as many different splitting as they like in the same code.
   4) The tensorial expression can then be split using the function 'SplitExpression'. This first expands out any curvature tensors into derivatives of the metric, preventing us from needing to follow the Gauss-Codazzi route which is intentional since we want to be more general than a 3+1 split. If any metrics are defined then these are used to put all tensor indices into their default positions (those positions as given at definition time). This is useful because the splitting of a tensor is dependent on the up/down character of the indices, and so by having all indices in default locations minimizes the number of split-tensors needed. Any dummy indices are then expanded out to be either parameters or indices of the defined sub-index ranges. If the input expression contains free indices then these are expanded into an array, with the dimension of the resulting array equal to the number of free indices. For example, let us define \[OpenCurlyQuote]MySplitting\[CloseCurlyQuote] as the '3+1' split the spacetime indices '\[Mu],\[Nu],...' become either a time parameter 't' or spatial indices 'i,j,...', where the spatial indices are defined on a manifold 'M3'. Using SplitExpression to perform MySplitting on a scalar quantity such as F[\[Mu],-\[Mu]] then yields F[t,-t]+F[i,-i]. This result contains invalid tensors because the parameter indices are inhomogeneous and not of standard form. This is resolved via an internal call to the function 'ToDefaultNomenclature' which we now discuss:
   5) 'ToDefaultNomenclature' acts on tensors with invalid parameter indices and, if not already defined, creates new split-tensors that are valid for the given indices. Following the previous example with a splitting called 'MySplitting', 'ToDefaultNomenclature' leads to the transformation F[t,-t]->F\[FormalCapitalS]MySplitting\[FormalCapitalU]t\[FormalCapitalD]t[] and F[i,-i]->F\[FormalCapitalS]MySplitting\[FormalCapitalM]M3\[FormalCapitalM]M3[i,-i], where the new tensors F\[FormalCapitalS]MySplitting\[FormalCapitalU]t\[FormalCapitalD]t and F\[FormalCapitalS]MySplitting\[FormalCapitalM]M3\[FormalCapitalM]M3 are in the 'default nomenclature' of the SplitExpression package. The default nomenclature is intended only as a placeholder; it may look ugly, but the tensor names clearly state how they are defined. If we have defined a splitting called 'MySplitting' and a tensor called 'MyTensor' that has 'N' indices being split, then the default nomenclature for the split-tensors of MyTensor is a concetenation of names as <MyTensor>\[FormalCapitalS]<MySplitting><IndexForm1><IndexForm2>...<IndexFormN>, where <IndexForm> takes one of three forms:
    
   a) If the index is a contravariant (up) parameter called MyParameter then IndexForm = \[FormalCapitalU]MyParameter.
   b) If the index is a covariant (down) parameter called MyParameter then IndexForm = \[FormalCapitalD]MyParameter.
   c) If the index is an index (up or down) on some manifold MyManifold then IndexForm = \[FormalCapitalM]MyManifold.
    
   We incorporate the name of the splitting in the tensor name to ensure that all split-tensors are unique to each given splitting.
   6) One is able to stick with the default nomenclature. However, this may not be ideal for two reasons: Firstly, the resulting expressions look ugly and are longwinded. Secondly, it is likely that one would want to write out the split-tensors in terms of other tensors (such as in the '3+1' split in Relativity where the time-time component of the metric is rewritten as a function of the Lapse scalar and the Shift vector). To allow this to be done easily, the function SplittingRules may be used. This sets up a list of rules that are appended to the global variable $SplittingRules[MySplitting]. These rules can then be applied to an expression by using the function 'UseSplittingRules'. UseSplittingRules is automatically called by SplitExpression, so if one defines the splitting rules in advance of splitting the tensorial expression, then the default nomencature is never output.
    
   SplitExpression works with covariant derivatives and curvature tensors in a particularly naive way, by expanding them all out in terms of partial derivatives of the metric and then splitting the indices. This has the benefit of being very simple, robust and widely applicable. The expressions therefore contain very many partial derivatives PD. The user may then want to rewrite these in terms of covariant derivatives defined using metrics of the spaces following the splitting, but since there does not appear to be a general way to perform this operation we have kept the PD derivatives in the output.
   
   For finding individual components of a tensorial expression, the auxilliary function SplitComponents may be helpful. This uses the SplitExpression function to make the split into individual components, and then arranges them in a column with labels to make the output more user-friendly. "


(* ::Subsection::Closed:: *)
(*Acknowledgements*)


(* ::Text:: *)
(*My thanks go to David Seery and the University of Sussex for allowing me to devote so much time into writing this package. *)
(*I am also indebted to Guido Pettinari for patiently sharing some of his vast knowledge of Mathematica.*)
(*--Joseph Elliston*)


(* ::Section::Closed:: *)
(*Public context*)


Unprotect[$Splittings];
$Splittings::usage = "Globally defined list of available splittings. If no splittings have been created, $Splittings={}.";

Unprotect[SplittingQ];
SplittingQ::usage = "SplittingQ[symbol] gives True if symbol is registered as a splitting and False otherwise.";

Unprotect[DecompositionQ];
DecompositionQ::usage = "DecompositionQ[OldManifold,Decomposition] gives True if Decomposition is of the correct form to be used as a decomposition argument to DefSplitting. DecompositionQ checks the form of Decomposition and checks that it has a total dimensionality equal to that of OldManifold. Decomposition must be a list of Manifolds or Parameters all of which must have been predefined.";

Unprotect[DefSplitting];
DefSplitting::usage = "DefSplitting[MySplitting,OldManifold->Decomposition] defines a prescription for splitting indices over smaller summations or individual parameters. Decomposition must be a list of Manifolds or Parameters all of which must have been predefined. We use parameters in this way to avoid the unwanted clutter indices that would otherwise live on single-dimensional manifolds. 'MySplitting' becomes a new splitting object and should not be previously defined.";
DefSplitting::exists = "The symbol `1` is already defined."

Unprotect[UndefSplitting];
UndefSplitting::usage = "UndefSplitting[MySplitting] undefines the Splitting called MySplitting.";

Unprotect[AllowDerivsOfUpMetric];
AllowDerivsOfUpMetric::usage = "AllowDerivsOfUpMetric[MyMetric] removes the default xTensor upvalue of MyMetric that rewrites derivatives of the contravariant metric in terms of derivatives of the covariant metric. Removing this rule is useful because then one can avoid introducing many extra metric terms that make expressions longer. This is particularly useful when the indices will then be split, where the calculation is often greatly simplified by minimising the number of dummy index pairs.";
AllowDerivsOfUpMetric::failmany = "Unexpected scenario encountered in function. This is because AllowDerivsOfUpMetric works on the principle that the upvalue of the metric `1` that it is trying to remove is the only upvalue that contains the FirstDerQ function, and instead multiple instances of this function have been found. AllowDerivsOfUpMetric needs a minor modification to enable it to select the appropriate UpValue.";
AllowDerivsOfUpMetric::warningnone = "The function AllowDerivsOfUpMetric[`1`] was called to remove a specific upvalue of the metric `1` but this upvalue was not present. This is not an error but may indicate the code is not working as intended.";
 
Unprotect[MinimizeMetricDummies];
MinimizeMetricDummies::usage = "MinimizeMetricDummies[MyExpression,MyMetric] reduces the length of terms in MyExpression involving derivatives of MyMetric. This function should only be used after AllowDerivsOfUpMetric[MyMetric] otherwise it will have no effect other than to waste computing time. By default, xTensor manipulates expressions to only yield derivatives acting on the covariant metric. This is implemented automatically by a rule defined as an upvalue of Metric. Whilst this is helpful in many circumstances, it generates lots of extra dummy indices which can vastly complicate the process of splitting indices. This default rule may be removed by using the function AllowDerivsOfUpMetric[MyMetric]. MinimizeMetricDummies then combines metric factors to shortern expressions as much as possible, up to double derivative terms of the metric.";

Unprotect[ToDefaultNomenclature];
ToDefaultNomenclature::usage = "ToDefaultNomenclature[MySplitting][MyExpression] is a sub-function of the SplitExpression function. As an input, MyExpression is the result of a naive splitting whereby indices in a particular VBundle are simply replaced with a set of other indices or parameters. Such an expression is not valid because the indices do not match the indice configuration of the original tensor. The input MyExpression is modified by the ToDefaultNomenclature function such that the output is valid, which involved the creation of new split tensors as required. These new split tensors are defined with the appropriate rank, by removing parameter indices at the point of definition, but other indices remain. The name of the resulting split-tensors describes the original index configuration in a verbose but reliable way. If we have defined a splitting called MySplitting and a tensor called MyTensor that has N indices being split by MySplitting, then the default nomenclature for a given split-tensor of MyTensor is a concetenation of names associated to its indices as <MyTensor>\[FormalCapitalS]<MySplitting><IndexForm1><IndexForm2>...<IndexFormN>, where <IndexForm> takes one of three forms:
            If the index is a contravariant (up) parameter called MyParameter then IndexForm = \[FormalCapitalU]MyParameter.
            If the index is a covariant (down) parameter called MyParameter then IndexForm = \[FormalCapitalD]MyParameter.
            If the index is an index (up or down) on some manifold MyManifold then IndexForm = \[FormalCapitalM]MyManifold.";
ToDefaultNomenclature::nsplitting = "`1` is not a valid splitting object defined with DefSplitting.";
ToDefaultNomenclature::multiple = "There should only be one argument in each of the two brackets of ToDefaultNomenclature[Splitting][Expression].";
ToDefaultNomenclature::null = "One or both of the arguments to ToDefaultNomenclature are empty. The correct method has two non-empty arguments as ToDefaultNomenclature[Splitting][Expression].";

Unprotect[SplitExpression];
SplitExpression::usage = "SplitExpression[MySplitting][MyExpression] splits indices in MyExpression according to the splitting MySplitting. If MyExpression contains N-free indices then the output is an N-dimensional array. The expansion will only work if the indices in expression that are in MySplitting's VBundle are abstract (A) indices as used by default in xTensor. This function expands out any curvature quantities in terms of the metric, which it then simplifies using the functions AllowDerivsOfUpMetric and MinimizeMetricDummies which reduces the number of dummy indices in expressions. SplitExpression then makes an internal call to ToDefaultNomenclature in order to produce valid split-tensors.";
SplitExpression::multiple = "There should only be one argument in each of the two brackets of SplitExpression[MySplitting][MyExpression].";
SplitExpression::null = "One or both of the arguments to SplitExpression are empty. The correct method has two non-empty arguments as SplitExpression[MySplitting][MyExpression].";
SplitExpression::invinds = "The input expression contains indices that are not supported as input arguments to the function SplitExpression. The expansion will only work for abstract (A) indices in the TangentVBundle being expanded, though other indices may exist in other VBundles or as labels.";
SplitExpression::deltafail = "The DownValues of the delta tensor have been altered by the action of SplitExpression.";

Unprotect[SplittingRulesListQ];
SplittingRulesListQ::usage = "Returns True if its argument is of the correct form for being a SplittingRulesList, otherwise resturns False. SplittingRulesListQ checks the form of the expression and its index syntax. All Tensors contained within SplittingRulesList are presumed to be already defined. SplittingRulesList should be a list of single depth and each element should be a rule of the form TensorName[Indices]->Result. 'Indices' are the indices of the tensor rule being defined, and these can be contravariant or covariant and can also be parameters. 'Result' should be tensorially correct (without any parameter indices) and must contain the same free (non-parameter) indices as 'Indices' in the same up/down configuration.";

Unprotect[SplittingRules];
SplittingRules::usage = "SplittingRules[MySplitting,MySplittingRulesList] allows the user to define rules for how the split-tensors will be rewritten after application of the SplitExpression function. This is useful because the default nomenclature for split-tensors is often longwinded and also one may want to write out the split-tensors in terms of other tensors. SplittingRules appends rules to the global variable $SplittingRules[MySplitting]. These rules are then applied to an expression by using the function 'UseSplittingRules'. See SplittingRulesListQ for the syntax needed for SplittingRulesList.";
SplittingRules::multiple = "Internal error: Multiple instances of the same transformation rule found: `1`.";

Unprotect[UseSplittingRules];
UseSplittingRules::usage = "UseSplittingRules[MySplitting][MyExpression] applies the rules $SplittingRules[MySplitting] on MyExpression. These rules are defined using the SplittingRules function and are used to rewrite the split-tensors in a form that is not the default nomenclature.";
UseSplittingRules::argx = "There should only be one argument in each of the two brackets of UseSplittingRules[MySplitting][MyExpression].";

Unprotect[SplitComponents];
SplitExpression::multiple = "There should only be one argument in each of the two brackets of SplitComponents[MySplitting][MyExpression].";
SplitExpression::null = "One or both of the arguments to SplitComponents are empty. The correct method has two non-empty arguments as SplitComponents[MySplitting][MyExpression].";
SplitComponents::usage = "SplitComponents[MyExpression] is identical to SplitExpression[MyExpression] except for the output formatting. When MyExpression has free indices, SplitExpression outputs an array whose dimension equals the number of free indices. This minimal output is useful when embedding the function SplitExpression in other code. However, if we just want to know the components of a tensor then a more verbose output formatting is desirable. SplitComponents outputs the components in a Column, showing what each part of the decomposition yields. ";

Unprotect[AddTensorDependencies];
AddTensorDependencies::usage = "AddTensorDependencies[{SomeTensors},{SomeParameters}] adds SomeParameters as dependencies of SomeTensors.";
AddTensorDependencies::invarg = "The input arguments are invalid. The first argument should be a list of tensors and the second argument a list of parameters, all of which have already been defined.";

Unprotect[InverseComposition];
InverseComposition::usage = "InverseComposition[expr] will output a list of the form {{Deriv1,Deriv2,Deriv3,...},Arg} where expr=Deriv1@Deriv2@Deriv3@...@Arg, where Deriv can be any covariant dderivative for which CovD returns True, or a parameter derivative ParamD. InverseComposition therefore strips derivatives off of the argument Arg and returns the result in a decomposed form. This process is essentailly the inverse of Mathematica's 'Composition' function, hence the name. For any argument that does not fit the description of being at least one derivative acting on an argument, the output will be {{},expr}.";


(* ::Text:: *)
(*The default value for $Splittings is an empty list.*)


$Splittings={};


(* ::Section::Closed:: *)
(*Private Context*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Debugging*)


(* ::Text:: *)
(*Define the variable InfoLevel to control the output of useful debugging messages. *)


(* ::Item:: *)
(*InfoLevel=0 --- All output messages are minimised.*)


(* ::Item:: *)
(*InfoLevel=1 --- Higher-level output messages are permitted.*)


(* ::Item:: *)
(*InfoLevel=2 --- All output messages are permitted.*)


InfoLevel = 0;


(* ::Text:: *)
(*Also define the function InfoPrint[stuff,level] that prints out 'stuff' only if InfoLevel is sufficient.*)


SetNumberOfArguments[InfoPrint,2];
InfoPrint[expr_,level_]:=If[level<=InfoLevel,Print[expr];];


(* ::Subsection::Closed:: *)
(*SplittingQ*)


(* ::Input:: *)
(*?SplittingQ*)


SetNumberOfArguments[SplittingQ,1];
(*Default result is False*)
SplittingQ[_]:=False;


(* ::Subsection::Closed:: *)
(*DecompositionQ*)


(* ::Input:: *)
(*?DecompositionQ*)


SetNumberOfArguments[DecompositionQ, 2];
(*Default case is False*)
DecompositionQ[_, _] := False;

DecompositionQ[OldManifold_?ManifoldQ, Decomp_] :=
    	TrueQ[
      		Head@Decomp === List
        		&&
        		(*Check the form of each term of SplitRules*)
        		0 == Total[(Boole@Not@MatchQ[#, _?ParameterQ | _?ManifoldQ] &) /@ Decomp]
        		&&
        		(*Check that no submanifold or parameter is used more than once*)
        		DuplicateFreeQ[Decomp]
        		&&
        		(*Check overall dimensionality matches before and after the proposed decomposition*)
        		Total[If[ParameterQ@#, 1, DimOfManifold@#] & /@ Decomp] == DimOfManifold@OldManifold
      	];


(* ::Subsection::Closed:: *)
(*DefSplitting*)


(* ::Input:: *)
(*?DefSplitting*)


SetNumberOfArguments[DefSplitting, 2];


(* ::Text:: *)
(*First define the case where all arguments are of the correct form*)


DefSplitting[splitting_Symbol, OldManifold_?ManifoldQ -> Decomp_List] :=
	Module[{},
		(*Basic properties associated with splitting*)
		SplittingQ[splitting] ^= True;
		Unprotect[$Splittings]; AppendTo[$Splittings, splitting]; Protect[$Splittings];
		ManifoldOf[splitting] ^= OldManifold;
		VBundleOf[splitting] ^= TangentBundleOfManifold@OldManifold;
		(*If a metric is not defined then the MetricOf and CovDOf upvalues are empty lists*)
		MetricOf[splitting] ^= If[# === {}, {}, First@#] &@MetricsOfVBundle@VBundleOf[splitting];
		CovDOf[splitting] ^= If[# === {}, {}, CovDOfMetric@#] &@MetricOf[splitting];
		(*At definition time there are no SplitTensors associated to a splitting*)
		SplitTensorsOf[splitting] ^= {};
      	
		(*Store the SubManifolds, parameters and SubVBundles of splitting as upvalues.*)
		SubManifoldsOf[splitting] ^= DeleteCases[Decomp, _?ParameterQ];
		ParametersOf[splitting] ^= DeleteCases[Decomp, _?ManifoldQ];
		SubVBundlesOf[splitting] ^= TangentBundleOfManifold /@ SubManifoldsOf[splitting];
	
	] /; (DecompositionQ[OldManifold, Decomp]) /; (!SplittingQ@splitting)


(* ::Text:: *)
(*Define error messages for invalid entries in the first argument*)


(*If splitting is an existing SplitExpression, remove it and begin again*)
DefSplitting[splitting_Symbol, OldManifold_?ManifoldQ -> Decomp_List] :=
  Module[{},	
      	Print[ToString@splitting <> " is already defined. Removing and redefining this splitting..."];
      	UndefSplitting[splitting];
      	DefSplitting[splitting, OldManifold -> Decomp];
      ] /; (DecompositionQ[OldManifold, Decomp])/; (SplittingQ@splitting)

(*First argument is an existing symbol that is not a splitting*)
DefSplitting[splitting_Symbol,OldManifold_?ManifoldQ -> Decomp_List]:= 
	Message[DefSplitting::exists, splitting] /; 
	(DecompositionQ[OldManifold, Decomp])

(*First argument is not a symbol*)
DefSplitting[NotSymbol_,OldManifold_?ManifoldQ -> Decomp_List] := 
	Message[DefSplitting::invalid, NotSymbol, "symbol"]/; 
	(DecompositionQ[OldManifold, Decomp]);


(* ::Text:: *)
(*Define usage messages for invalid entries in the second argument*)


(*Second argument of correct form but incorrect decomposition specified*)
DefSplitting[_,OldManifold_?ManifoldQ -> Decomp_List] := 
	Message[DefSplitting::invalid, OldManifold -> Decomp, "decomposition. The decomposition must yield True when acted on by DecompositionQ. See the usage message for DecompositionQ for details of the appropriate syntax."]

(*Second argument does not contain a valid manifold as the LHS of the rule*)
DefSplitting[_, OldManifold_->Decomp_List] := Message[DefSplitting::invalid, OldManifold, "manifold"];

(*Second argument does not contain a valid list as the RHS of the rule*)
DefSplitting[_, _->Decomp_] := Message[DefSplitting::invalid, Decomp, "list"]

(*Second argument is not a rule*)
DefSplitting[_, NotRule_] := Message[DefSplitting::invalid, NotRule, "rule yielding True when acted on by DecompositionQ. See the usage message for DecompositionQ for details of the appropriate syntax."]


(* ::Subsection::Closed:: *)
(*UndefSplitting*)


(* ::Input:: *)
(*?UndefSplitting*)


SetNumberOfArguments[UndefSplitting, 1];


(* ::Text:: *)
(*First define the case where all arguments are of the correct form*)


UndefSplitting[splitting_?SplittingQ] :=
	Module[{ii},
		(*Remove splitting from the global list $Splittings*)
		InfoPrint["Initially $Splittings = ",2];
		InfoPrint[$Splittings,2];		
		Unprotect[$Splittings];
		$Splittings = DeleteCases[$Splittings, splitting];
		Protect[$Splittings];

		(*Remove any VisitorsOf[] dependencies for the split tensors*)
		For[ii = 1, ii <= Length@SplitTensorsOf[splitting], ii++,
			xAct`xTensor`Private`DropFromHosts[SplitTensorsOf[splitting][[ii]]];
			(*Undefing the splitting tensors*)
			UndefTensor[SplitTensorsOf[splitting][[ii]]];
		];

		(*Remove any SplittingRules*)
		If[ValueQ@Global`$SplittingRules[splitting],
			Global`$SplittingRules[splitting] =.;
		];

		(*Remove splitting*)
		ClearAll[splitting];
		InfoPrint["Finally $Splittings = ",2];
		InfoPrint[$Splittings,2];		
	];


(* ::Text:: *)
(*Then the case there the argument is incorrect:*)


(*Invalid argument case*)
UndefSplitting[NotSplitting_] := Message[UndefSplitting::invalid, NotSplitting, "splitting"];


(* ::Subsection::Closed:: *)
(*AllowDerivsOfUpMetric*)


(* ::Input:: *)
(*?AllowDerivsOfUpMetric*)


SetNumberOfArguments[AllowDerivsOfUpMetric, 1];


(* ::Text:: *)
(*First define the case where all of the arguments are of the correct form:*)


AllowDerivsOfUpMetric[metric_?MetricQ] :=
	Module[{UpValuePos, RemovedUpValue, ProtectedQ},
		(*Find and remove the upvalue for the metric that expands out derivatives of the contravariant metric.
		If it is not there, then this just means that the output a message saying that nothing has happened.
		We use the fact that the rule to be removed is the only rule involving the FirstDerQ function.*)
		UpValuePos = First /@ Position[UpValues[metric], FirstDerQ];
		InfoPrint["Position of updavlue of metric to remove is " <> ToString@UpValuePos, 2]; 

		(*First the correct case where only one such upvalue exists*)
		If[Length@UpValuePos == 1,
			(*Identify the removed upvalue and output this if InfoLevel=2*)
			RemovedUpValue = UpValues[metric][[(UpValuePos[[1]])]];
			InfoPrint["Removing the following UpValue of the metric " <> ToString@metric <> " :" <> ToString@RemovedUpValue, 2]; 
			(*Check if 'metric' is protected and make sure it is left in the same way albeit without the upvalue*)
			ProtectedQ = MemberQ[Attributes@metric, Protected];
    		If[ProtectedQ, Unprotect@metric];
    		UpValues[metric] = Delete[UpValues[metric], UpValuePos];
    		If[ProtectedQ, Protect@metric];
    	];

		(*If no value is returned then presume that this means that the function has already been run*)
		If[Length@UpValuePos == 0,	
			InfoPrint["There was no upvalue to remove in the AllowDerivsOfUpMetric function",2]
		];

		(*If more than one instance of FirstDerQ is found then this function needs to be altered and so we generate a message now.*)
		If[Length@UpValuePos > 1,	
			Message[AllowDerivsOfUpMetric::failmany, metric]
		];
	];


(* ::Text:: *)
(*Now define the case where the argument is not a valid metric*)


AllowDerivsOfUpMetric[NotMetric_] := Message[AllowDerivsOfUpMetric::invalid, NotMetric, "metric"];


(* ::Subsection::Closed:: *)
(*MinimizeMetricDummies*)


(* ::Input:: *)
(*?MinimizeMetricDummies*)


SetNumberOfArguments[MinimizeMetricDummies, 2];


(* ::Text:: *)
(*First define the case where all of the arguments are of the correct form:*)


MinimizeMetricDummies[expr_, g_?MetricQ] :=
  Module[{rule1, newexpr, rule2},

   	(*rule1 simplifies terms involving single derivatives of the metric.
		The derivative is 'Op' which is either a CovD or a ParamD.
		'stuff' accounts for the possibility that there are terms stuck in the middle of the expression*)		
   	rule1 = {
     	g[a_, b_]*stuff___*g[c_, d_]*Op_[e_]@g[f_, i_] /; Sort[-{f, i}] === Sort[{a, c}]
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i}, False] == 0) 
			:> -stuff*Op[e]@g[b, d],
     	g[a_, b_]*stuff___*g[c_, d_]*Op_[e_]@g[f_, i_] /; Sort[-{f, i}] === Sort[{b, c}] 
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i}, False] == 0) 
			:> -stuff*Op[e]@g[a, d],
     	g[a_, b_]*stuff___*g[c_, d_]*Op_[e_]@g[f_, i_] /; Sort[-{f, i}] === Sort[{a, d}]
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i}, False] == 0) 
			:> -stuff*Op[e]@g[b, c],
     	g[a_, b_]*stuff___*g[c_, d_]*Op_[e_]@g[f_, i_] /; Sort[-{f, i}] === Sort[{b, d}]
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i}, False] == 0) 
			:> -stuff*Op[e]@g[a, c]
     	};

   	(*rule for simplifying double derivatives of the metric*)
   	rule2 = {
     	g[a_, c_]*stuff1___*g[b_, d_]*stuff2___*Op_[e_]@Op_[f_]@g[i_, j_] /; Sort[-{i, j}] === Sort[{c, d}] 
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i, j}, False] == 0) 
			:> stuff1*stuff2 (-Op[e]@Op[f]@g[a, b] - g[a, c] Op[e]@g[b, d] Op[f]@g[-c, -d] - g[b, d] Op[e]@g[a, c] Op[f]@g[-c, -d]),
     	g[a_, c_]*stuff1___*g[b_, d_]*stuff2___*Op_[e_]@Op_[f_]@g[i_, j_] /; Sort[-{i, j}] === Sort[{a, d}] 
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i, j}, False] == 0) 
			:> stuff1*stuff2 (-Op[e]@Op[f]@g[c, b] - g[c, a] Op[e]@g[b, d] Op[f]@g[-a, -d] - g[b, d] Op[e]@g[c, a] Op[f]@g[-a, -d]),
     	g[a_, c_]*stuff1___*g[b_, d_]*stuff2___*Op_[e_]@Op_[f_]@g[i_, j_] /; Sort[-{i, j}] === Sort[{c, b}] 
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i, j}, False] == 0) 
			:> stuff1*stuff2 (-Op[e]@Op[f]@g[a, d] - g[a, c] Op[e]@g[d, b] Op[f]@g[-c, -b] - g[d, b] Op[e]@g[a, c] Op[f]@g[-c, -b]),
     	g[a_, c_]*stuff1___*g[b_, d_]*stuff2___*Op_[e_]@Op_[f_]@g[i_, j_] /; Sort[-{i, j}] === Sort[{a, b}] 
			/; (CovDQ@Op || ParamD === Op) /; (Count[ABIndexQ /@ {a, b, c, d, e, f, i, j}, False] == 0) 
			:> stuff1*stuff2 (-Op[e]@Op[f]@g[c, d] - g[c, a] Op[e]@g[d, b] Op[f]@g[-a, -b] - g[d, b] Op[e]@g[c, a] Op[f]@g[-a, -b])
     	};
   	   
   	(*apply these rules repeatedly on expr until no further changes happen*)
   	newexpr = FixedPoint[Expand[ContractMetric[ToCanonical[Expand[#] /. rule2, UseMetricOnVBundle -> None]]] &, expr];
   	newexpr = FixedPoint[Expand[ContractMetric[ToCanonical[Expand[#] /. rule1, UseMetricOnVBundle -> None]]] &, newexpr];
   	newexpr // ScreenDollarIndices
   ];


(* ::Text:: *)
(*Now define the case where the argument is not a valid metric*)


MinimizeMetricDummies[expr_, g_] := Message[MinimizeMetricDummies::invalid, g, "metric"];


(* ::Subsection::Closed:: *)
(*ToDefaultNomenclature*)


(* ::Input:: *)
(*?ToDefaultNomenclature*)


(* ::Text:: *)
(*Set the number of arguments:*)


(*Cases where one or both arguments are null*)
ToDefaultNomenclature[][] := Message[ToDefaultNomenclature::null];
ToDefaultNomenclature[__][] := Message[ToDefaultNomenclature::null];
ToDefaultNomenclature[][__] := Message[ToDefaultNomenclature::null];
(*Multiple arguments*)
ToDefaultNomenclature[_, A__][_] := Message[ToDefaultNomenclature::multiple];
ToDefaultNomenclature[_][_, A__] := Message[ToDefaultNomenclature::multiple];


(* ::Text:: *)
(*Define the case where all works properly. *)


ToDefaultNomenclature[splitting_?SplittingQ][expr_] := Module[{newexpr},
   	newexpr = expr;
   	(*Almost the entire content of this function is the renaming of tensors to default nomenclature using the following rule:
   	Note that this applies so long as the tensor "AA" includes one or more indices that were affected by "Splitting".
		This dependency is determined using the HostsOf function which relates "AA" to its dependent VBundles*)
   	newexpr = newexpr /. AA_[BB__] /; (xTensorQ@AA && MemberQ[HostsOf@AA, VBundleOf[splitting]]) :>
      		Module[{OldVBPositions, ChangedVBPositions, NewName, NewDependencies, NewTensorInds, NonDummyInds},
       			
       			(*OldVBPositions gives the locations of the indices that were originally in VBundleOf[splitting]*)
       			OldVBPositions = Position[SlotsOfTensor[AA] /. -CC_ :> CC, VBundleOf[splitting]];
       
       			(*NewName gives the name of the tensor that AA[BB] will turn into in default nomenclature*)
       			NewName = SymbolJoin[AA, "\[FormalCapitalS]", splitting, StringJoin @@ ToString /@ (Switch[#,
               					-_?ParameterQ, StringJoin["\[FormalCapitalD]", ToString[-#]],
               					_?ParameterQ, StringJoin["\[FormalCapitalU]", ToString[#]],
               					_?AIndexQ, StringJoin["\[FormalCapitalM]", ToString[BaseOfVBundle@VBundleOfIndex@#]]
               				] & /@ Extract[{BB}, OldVBPositions])];
       
       			(*NewDependencies gives the manifold or parameter dependencies of the new tensor*)
       			(*This includes all parameter dependencies by default*)
       			NewDependencies = DeleteDuplicates@Join[
          				ParametersOf[splitting],
          				Switch[#, -_?ParameterQ, -#, _?ParameterQ, #, _?AIndexQ, BaseOfVBundle@VBundleOfIndex@#] & /@ {BB}
          				];
       
				   (*NewTensorInds gives the indices that the tensor will take after writing in default nomenclature*)
       			(*This is simple, it is just the original indices minus any paramter indices*)
       			NewTensorInds = Delete[{BB}, Position[TrueQ[ParameterQ@# || ParameterQ@(-#)] & /@ {BB}, True]];
                     
					(*If the tensor has not already been defined, define it*)
					If[!xTensorQ@NewName,
						InfoPrint["Defining the tensor " <> ToString@NewName, 1];
						(*We must ensure that the indices being used to define the tensor are not dummy indices*)
						(*The second factor below gives the NonDummyInds the correct sign*)
						NonDummyInds = ((NewIndexIn@VBundleOfIndex@#) (2*Boole@UpIndexQ@# - 1)) & /@ NewTensorInds;
						DefTensor[NewName @@ NonDummyInds, NewDependencies];
						(*Update the SplitTensorsOf upvalues of the splitting*)
						SplitTensorsOf[splitting] ^= DeleteDuplicates@Join[SplitTensorsOf[splitting], {NewName}];
					];
					
					(*Finally, the output is just the new tensor name and its new indices*)
                    NewName @@ NewTensorInds
				];
   
   	(*Now just change any PD derivatives that have parameter indices for parameter derivatives*)
   	newexpr = newexpr //. PD[-BB_?ParameterQ][CC__] :> ParamD[BB][CC];
   	(*Arrange these derivatives so that the PD derivatives act first*)
   	newexpr = newexpr //. PD[A_]@ParamD[B_]@C_ :> ParamD[B]@PD[A]@C
   ];


(* ::Text:: *)
(*Case where first argument is not a valid splitting*)


ToDefaultNomenclature[nsplitting_][expr_] := Message[ToDefaultNomenclature::unknown, "splitting", nsplitting];


(* ::Subsection::Closed:: *)
(*SplitExpression*)


(* ::Input:: *)
(*?SplitExpression*)


(* ::Text:: *)
(*Set the number of arguments:*)


(*Cases where one or both arguments are null*)
SplitExpression[][] := Message[SplitExpression::null];
SplitExpression[__][] := Message[SplitExpression::null];
SplitExpression[][__] := Message[SplitExpression::null];
(*Multiple arguments*)
SplitExpression[_, A__][_] := Message[SplitExpression::multiple];
SplitExpression[_][_, A__] := Message[SplitExpression::multiple];


(* ::Text:: *)
(*Case when all arguments are correct:*)


SplitExpression[splitting_?SplittingQ][expr_] :=
  Module[{newexpr, ii, dummies, NewInds, ExpandFirstDummy, Cleanednewexpr, free, xx, IndArray, tmpdelta},
    
    	(*MANIPULATE INPUT PRIOR TO DECOMPOSITION*)
    	newexpr = expr;
    	(*move all tensors indices into their default index positions*)
    	newexpr = newexpr // SeparateMetric[] // NoScalar // SameDummies // ScreenDollarIndices;
    	InfoPrint["After putting all tensor indices in default up/down positions one has:", 2];
    	InfoPrint[newexpr, 2];
    
    	(*If a metric has been defined associated to the tangentVBundle being split...*)
    	If[UnsameQ[MetricOf[splitting], {}],
     		(*We first remove one of the upvalues for the metric, to allow derivatives of the contravariant metric.*) 
     		AllowDerivsOfUpMetric[MetricOf[splitting]];
     		
     		(*Expand out any Einstein tensors associated to CovDOf[splitting].*)
     		newexpr = newexpr // EinsteinToRicci // Simplification;
     		InfoPrint["After expanding any Einstein tensors one has:", 2];
     		InfoPrint[newexpr // ScreenDollarIndices // Validate, 2];
     
     		(*Lower any contravariant derivatives*)
     		newexpr = newexpr //. (CovDOf[splitting])[a_][A__] /; (UpIndexQ@a && AIndexQ@a) :> Module[{tmp},
            			(MetricOf[splitting][a, tmp] (CovDOf[splitting])[-tmp]@A /. tmp -> NewIndexIn[VBundleOf[splitting]])
            			] // ReplaceDummies // SameDummies // ScreenDollarIndices;
     		InfoPrint["After lowering any contravariant derivatives one has:", 2];
     		InfoPrint[newexpr // ScreenDollarIndices // Validate, 2];
     
     		(*Expand out any covariant derivatives associated with the given VBundle as partial derivatives PD*)
     		newexpr = ToCanonical[ChangeCovD[newexpr, CovDOf[splitting], PD], UseMetricOnVBundle -> None] // Simplify;
     		InfoPrint["After changing from covariant to partial derivatives one has:", 2];
     		InfoPrint[newexpr // ScreenDollarIndices // Validate, 2];
     		
     		(*Expand out any Riemann/Ricci terms into partial derivatives of the metric.*)
     		newexpr = ToCanonical[ChangeCurvature[newexpr, CovDOf[splitting], PD] // NoScalar, UseMetricOnVBundle -> None] // SeparateMetric[];
     		InfoPrint["After expanding curvature tensors one has:", 2];
     		InfoPrint[newexpr // ScreenDollarIndices // Validate, 2];
     
     		(*Expand out any Christoffels associated to CovDOf[splitting]. This generates further partial derivatives.
     		Then use the internal function MinimizeMetricDummies to simplify this expression*)
     		newexpr = ToCanonical[ChristoffelToGradMetric[newexpr, MetricOf[splitting]], UseMetricOnVBundle -> None] // ContractMetric;
     		newexpr = MinimizeMetricDummies[newexpr, MetricOf[splitting]] // SeparateMetric[] // ScreenDollarIndices;
     		InfoPrint["After expanding Christoffels and simplifying one has:", 2];
     		InfoPrint[newexpr // ScreenDollarIndices // Validate, 2];
     	];
    	
    	(*DECOMPOSITION OF DUMMY INDICES*)
    	(*dummies is the list of remaining contravariant dummies in newexpr*)
    	dummies = List @@ IndicesOf[VBundleOf[splitting], Dummy, Up][newexpr];
    	(*Define the function ExpandFirstDummy that takes a term and expands out a given pair of indices dum, if they exist.*)
    	ExpandFirstDummy[term_, dum_] :=
     	Module[{NewVBInds, NewDumInds},
      		If[Position[term, dum] === {},
       			term,
       			(*NewVBInds gives new contravariant A-indices in the subTangent spaces associated to the given SplitExpression*)
       			NewVBInds = NewIndexIn /@ SubVBundlesOf[splitting];
       			(*NewDumInds also adds in the parameters as additional indices*)
       			NewDumInds = Join[ParametersOf[splitting], NewVBInds];
       			Sum[term /. dum -> NewDumInds[[kk]], {kk, 1, Length@NewDumInds}]
       		]			
      	];
    	(*Loop over each dummy and apply ExpandFirstDummy on each term on each loop*)
    	For[ii = 1, ii <= Length@dummies, ii++,
     		(*Now apply ExpandFirstDummy to each term of newexpr recursively until it has finished*)
     		newexpr = If[Head@newexpr === Plus,
       			ExpandFirstDummy[#, dummies[[ii]]] & /@ Expand[newexpr],
       			ExpandFirstDummy[newexpr, dummies[[ii]]]
       		];
     		InfoPrint["After expanding one iteration of the dummy indices one has:", 2];
     		InfoPrint[newexpr, 2];
     	];
    	InfoPrint["After expanding all of the dummy indices one has:", 1];
    	InfoPrint[newexpr, 1];
    
    	(*DECOMPOSITION OF FREE INDICES VIA A MULTI-DIMENSIONAL ARRAY*)
    	(*'free' are the free indices in the relevant VBundle. We can use IndicesOf to find these,
    	but first we must temporarily remove any parameter indices and parameter derivatives 
    	since these cause Validate to throw error messages:*)
    	Cleanednewexpr = (newexpr //. A_[B__] 
			/; xTensorQ@A
			/; MemberQ[{B}, -_?ParameterQ | _?ParameterQ] 
			:> A@@DeleteCases[{B},-_?ParameterQ | _?ParameterQ]
		) //. PD[_?ParameterQ | -_?ParameterQ][C__] :> C;
    	free = List @@ IndicesOf[Free, VBundleOf[splitting]][Cleanednewexpr];
    	(*Only compute the rest of this section if there are some free indices*)
    	If[Length@free =!= 0,
     		(*We now replace each free indice with all possible indices of the SubVBundles of the splitting.
			We mimmic the way the calculation would be done by hand, where each free indice is decomposed into a unique set of indices associated to 
			the various SubVBundles of the splitting. To make the multi-dimensional array of new indices, we will use the 'Outer' function
			but we must first create separate lists of replacement indices for each free index, with no re-use of indices between terms.*)
     		NewInds = Table[
				(*'xx' counts through the free indices. Each one is assigned the xx'th defined index in each SubVBundle, 
				unless that does not exist in which case a new index is generated*)
				(*Find out if there is an index already defined *)
       			If[Length@First@IndicesOfVBundle@# >= xx,
						(*If so, use it*)
          				Part[First@IndicesOfVBundle@#, xx],
						(*If not, get a new index*)
          				NewIndexIn@#
          			] & /@ SubVBundlesOf[splitting]
       		, {xx, 1, Length@free}];
     		(*Now prefix each list with the parameter names*)
     		NewInds = Join[ParametersOf[splitting], #] & /@ NewInds;
     		InfoPrint["New indices to replace the free indices are: " <> ToString@NewInds, 2];
     		(*Now we form an array of dimension equal to the number of free indices, which holds NewInds*)
     		IndArray = Outer[List, Sequence @@ NewInds];
     		InfoPrint["The array of replacement indices, before sign corrections, is: " <> ToString@IndArray, 2];
     		(*Make each index have the same sign as the original free index*)
			(*Map only at the bottom level in IndArray, where each set of indices should inherit the same signs as the original free indices*)
     		IndArray = Map[
				Times[If[UpIndexQ@#, 1, -1] & /@ free, #] &, 
				IndArray, 
				{Length@free}
			];
     		InfoPrint["New index array to replace the free indices is: " <> ToString@IndArray, 2];
     		(*Turn these indices into indice replacement rules*)
     		IndArray = Map[Thread[free -> #] &, IndArray, {Length@free}];
     		
			(*When the SplitComponents function is running, extract the index replacement rules here*)
     		If[SplitComponentsBoolean === True, ComponentRules = Nest[Flatten[#, 1] &, IndArray, Length@free - 1];];
     		InfoPrint["Index replacement rules are: " <> ToString@IndArray, 1];
     		
			(*When we substitute these indices, none of the tensors complain except for the delta tensor 
     		which tries to implement various automatic rules and in the process discovers the non-standard 
     		index forms and starts complaining. Resolving this by changing the properties of delta proves 
     		to be complicated. A simple, robust and ugly resolution is to temporarily replace delta with a new 
     		temporary tensor called tmpdelta. Define this and its output rules:*)
     		DefTensor[tmpdelta[], {}, PrintAs -> "\[GothicCapitalT]\[Delta]"];
				(*identity*)
     		tmpdelta /: tmpdelta[x_, -x_] := 1 /; (ParameterQ@x || AIndexQ@x);
     		tmpdelta /: tmpdelta[-x_, x_] := 1 /; (ParameterQ@x || AIndexQ@x);
				(*zero for mixed parameters or mixed VBundles which are not of the same type*)
     		tmpdelta /: tmpdelta[_?ParameterQ, -_?AIndexQ] := 0;
     		tmpdelta /: tmpdelta[-_?ParameterQ, _?AIndexQ] := 0;
     		tmpdelta /: tmpdelta[_?AIndexQ, -_?ParameterQ] := 0;
     		tmpdelta /: tmpdelta[-_?AIndexQ, _?ParameterQ] := 0;
     		tmpdelta /: tmpdelta[x_?AIndexQ, -y_?AIndexQ] := 0 /; (VBundleOfIndex@x =!= VBundleOfIndex@y);
     		tmpdelta /: tmpdelta[-x_?AIndexQ, y_?AIndexQ] := 0 /; (VBundleOfIndex@x =!= VBundleOfIndex@y);
     		tmpdelta /: tmpdelta[x_?ParameterQ, -y_?ParameterQ] /; x =!= y := 0;
     		tmpdelta /: tmpdelta[-x_?ParameterQ, y_?ParameterQ] /; x =!= y := 0;
     		
			(*switch delta for tmpdelta in the expression*)
     		newexpr = newexpr /. delta[a_, b_] :> tmpdelta[a, b];
     		InfoPrint["After changing delta to tmpdelta one has:", 2];
     		InfoPrint[newexpr, 2];
     		(*Now we can finally replace the free indices*)
     		newexpr = Map[ReplaceIndex[Evaluate[newexpr], #] &, IndArray, {Length@free}];
     		InfoPrint["After substituting for the free indices one has:", 2];
     		InfoPrint[newexpr, 2];
     		(*Returning to the original delta tensor as the indices have valid forms, and undefine the delta tensor*)
     		newexpr = newexpr /. tmpdelta -> delta;
     		InfoPrint["After replacing tmpdelta with delta one has:", 2];
     		InfoPrint[newexpr, 2];
     		UndefTensor[tmpdelta];
     	
     		InfoPrint["Result after splitting the free indices is:", 1];
     		InfoPrint[newexpr, 1];
     	];
    	
    	(*SWITCH TO AUTOMATED DECOMPOSED TENSOR NOMENCLATURE*)
    	newexpr = Map[ToDefaultNomenclature[splitting][#] &, newexpr, {Length@free}];
    	InfoPrint["After switching to default nomenclature we have:", 2];	
    	InfoPrint[newexpr, 2];
    	newexpr = UseSplittingRules[splitting][newexpr];
    	InfoPrint["After applying the splitting rules we have:", 1];	
    	InfoPrint[newexpr, 1];
    	(*Output the result*)
    	newexpr = Map[ScreenDollarIndices@ReplaceDummies@NoScalar@ToCanonical[#,UseMetricOnVBundle->None]&, newexpr, {Length@free}]
    
    ] /; (IndicesOf[VBundleOf[splitting]][expr] === IndicesOf[VBundleOf[splitting], AIndex][expr]);


(* ::Text:: *)
(*The case where splitting is not an existing Splitting*)


SplitExpression[splitting_?(Composition[Not, SplittingQ])][_] := Message[SplitExpression::unknown, "splitting", splitting]


(* ::Text:: *)
(*Any other scenario is presumed to mean that  expr is not of the correct A-index only form:*)


SplitExpression[_][expr_] := Message[SplitExpression::invinds];


(* ::Subsection::Closed:: *)
(*SplittingRulesListQ*)


(* ::Input:: *)
(*?SplittingRulesListQ*)


(* ::Text:: *)
(*Set the number of arguments:*)


SetNumberOfArguments[SplittingRulesListQ, 1];


(* ::Text:: *)
(*Define the case where all works properly. *)


SplittingRulesListQ[SplittingRulesList_List] :=
   TrueQ[
		(*Check form of each term*)
    	0 == Count[MatchQ[#, Rule[_?xTensorQ[___], __]] & /@ SplittingRulesList, False]
		&&
		(*Check that the indices on the LHS are parameters or Aindices*)
		{} === Flatten[(DeleteCases[List @@ First@#, -_?ParameterQ | _?ParameterQ | _?AIndexQ]) & /@ SplittingRulesList]
		&&
		(*Check that each free non-parameter index on LHS matches the free indices on RHS unless the RHS is equal to zero*)
		Module[{CleanedRules},
		CleanedRules = DeleteCases[SplittingRulesList, -_?ParameterQ | _?ParameterQ, Infinity];
			0 == Count[If[Last@# === 0, True, IndicesOf[Free]@First@# === IndicesOf[Free]@Last@#] & /@ CleanedRules, False]
		]
    ];


(* ::Text:: *)
(*Default case for other scenarios:*)


SplittingRulesListQ[_] := False;


(* ::Subsection::Closed:: *)
(*SplittingRules*)


(* ::Input:: *)
(*?SplittingRules*)


(* ::Text:: *)
(*Set the number of arguments:*)


SetNumberOfArguments[SplittingRules, 2];


(* ::Text:: *)
(*Define the case where all works properly. *)


SplittingRules[splitting_?SplittingQ, SplittingRulesList_?SplittingRulesListQ] :=
    Module[{ii, term, default, LHSpattern, pos},
      	(*If $SplittingRules[splitting] is not defined, make it an empty list*)
      	If[Unevaluated[Global`$SplittingRules[splitting]] === Global`$SplittingRules[splitting], Global`$SplittingRules[splitting] = {}];
      	(*Cycle through each term in SplittingRulesList*)
      	For[ii = 1, ii <= Length@SplittingRulesList, ii++,
        		term = SplittingRulesList[[ii]];
        		(*Use the ToDefaultNomenclature function to define new tensors as required*)
        		default = ToDefaultNomenclature[splitting][First@term];
        		(*Define the pattern that will form the LHS of the argument*)
        		LHSpattern = Switch[#,
                			-_?AIndexQ, -PatternTest[Pattern[Evaluate[-#], Blank[]], AIndexQ],
                			_?AIndexQ, \!\(\*
TagBox[
StyleBox[
RowBox[{"PatternTest", "[", 
RowBox[{
RowBox[{"Pattern", "[", 
RowBox[{"#", ",", 
RowBox[{"Blank", "[", "]"}]}], "]"}], ",", "AIndexQ"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)
                		] & /@ default;

        		(*We wish to make SplittingRules such that new rules will overwrite old rules. I had great difficulty in 
        		getting Mathematica to compare two terms which contain patterns. The only way that I have succeeded here 
        		is to send the patterns to FullForm and then to strings. This is robust, but hardly elegant. 
        		If you can do better, please email JME.*)

        		(*pos is the position of the terms of Global`$SplittingRules[splitting] that clash with LHSpattern*)
        		pos = Flatten@Position[MatchQ[ToString@FullForm@First@First@#, ToString@FullForm@Evaluate@LHSpattern] & /@ Global`$SplittingRules[splitting], True];
        		(*pos should have zero or 1 entry. Anything else signifies an internal error.*)
        		If[Length@pos > 1, Message[SplittingRules::multiple, Global`$SplittingRules[splitting][[pos]]]];
        		(*If there is a single term that clashes then remove this*)
        		Global`$SplittingRules[splitting] = Delete[Global`$SplittingRules[splitting], pos];
        		
        		(*add the new term, of the form HoldPattern[LHS]=Module[{dummies},RHS], achieved through the IndexRuleDelayed function*)
        		AppendTo[Global`$SplittingRules[splitting],
          			IndexRuleDelayed[
            				Evaluate[LHSpattern],
            				Evaluate[ScreenDollarIndices[Last[term] // SeparateMetric[]]]
            			]
          		];
        	];
      ];


(* ::Text:: *)
(*Case where first argument is not a valid splitting*)


SplittingRules[nsplitting_, SplittingRulesList_?SplittingRulesListQ] := Message[SplittingRules::unknown, "splitting", nsplitting];


(* ::Text:: *)
(*Any other case means that the second argument is not a valid splitting rules list:*)


SplittingRules[_, SplittingRulesList_] := Message[SplittingRules::unknown, "SplittingRulesList", SplittingRulesList];


(* ::Subsection::Closed:: *)
(*UseSplittingRules*)


(* ::Text:: *)
(*Set the number of arguments:*)


UseSplittingRules[][___] := Message[UseSplittingRules::argx];
UseSplittingRules[___][] := Message[UseSplittingRules::argx];
UseSplittingRules[_, A__][_] := Message[UseSplittingRules::argx];
UseSplittingRules[_][_, A__] := Message[UseSplittingRules::argx];


(* ::Text:: *)
(*Actual function, and all that this does is to apply splitting rules if they are defined.*)


UseSplittingRules[splitting_?SplittingQ][expr_]:=
Module[{},
	(*Check that there are rules to use*)
	If[!ValueQ@Global`$SplittingRules[splitting],
		expr,
		NoScalar[ToCanonical[(expr/.Power[A_,B_]:>Power[Scalar[A],B])/.Global`$SplittingRules[splitting],UseMetricOnVBundle->None]]//ScreenDollarIndices
	]
];


(* ::Text:: *)
(*The case where splitting is not an existing Splitting*)


UseSplittingRules[nsplitting_][_] := Message[UseSplittingRules::unknown, "splitting", nsplitting];


(* ::Subsection::Closed:: *)
(*SplitComponents*)


(* ::Input:: *)
(*?SplitComponents;*)


(* ::Text:: *)
(*Set number of arguments*)


(*Cases where one or both arguments are null*)
SplitComponents[][] := Message[SplitComponents::null];
SplitComponents[__][] := Message[SplitComponents::null];
SplitComponents[][__] := Message[SplitComponents::null];
(*Multiple arguments*)
SplitComponents[_, A__][_] := Message[SplitComponents::multiple];
SplitComponents[_][_, A__] := Message[SplitComponents::multiple];


(* ::Text:: *)
(*Actual function, and error trapping should be done via the SplitExpression function.*)


SplitComponents[splitting_][expr_] :=
  Module[{result, rules, LHS},
   	(*Extract the split expression and the index rules from the SplitExpression function.
   	This is done this using a boolean variable that is part of the SplitExpression functiion. This is not ideal but it works.*)
   	SplitComponentsBoolean = True;
   	result = Flatten@SplitExpression[splitting][expr];
   	ClearAll@SplitComponentsBoolean;
   	(*The component rules are output fom the Splitexpression function as ComponentRules. Move this to a local variable*)
		rules = ComponentRules;
   	ClearAll@ComponentRules;
   	(*now use rules and result to process the output*)
   	LHS = expr /. # & /@ rules;
   	ColumnForm@Thread[Rule[LHS, result]]	
   ];


(* ::Subsection::Closed:: *)
(*AddTensorDependencies function*)


SetNumberOfArguments[AddTensorDependencies, 2];


(* ::Text:: *)
(*Case where the function works correctly;*)


AddTensorDependencies[tensors_List, params_List] :=
 Module[{ii},
    	Unprotect[DependenciesOfTensor];
    	For[ii = 1, ii <= Length@tensors, ii++,
     		DependenciesOfTensor[tensors[[ii]]] ^= Join[DependenciesOfTensor[tensors[[ii]]], params];
     	];
    	Protect[DependenciesOfTensor];
    ] /; ! MemberQ[xTensorQ /@ tensors, False] /; ! MemberQ[ParameterQ /@ params, False]


(* ::Text:: *)
(*Cases for invalid arguments*)


AddTensorDependencies[tensors_List, params_List] /; ! MemberQ[xTensorQ /@ tensors, False] := Message[AddTensorDependencies::unknown,"list of parameters",params];
AddTensorDependencies[tensors_List, params_List] := Message[AddTensorDependencies::unknown,"list of tensors",params];
AddTensorDependencies[tensors_List, params_] := Message[AddTensorDependencies::unknown,"list",params];
AddTensorDependencies[_, _] := Message[AddTensorDependencies::invarg];


(* ::Subsection::Closed:: *)
(*InverseComposition*)


(* ::Input:: *)
(*?InverseComposition*)


SetNumberOfArguments[InverseComposition,1];


InverseComposition[expr_]:=
Module[{Arg,DerivList,DerRemover},
	(*Set default output values*)
	Arg=expr;
	DerivList={};
	(*Define DerRemover as the function that strips derivatives off Arg onion-style and appends these to DerivList*)
	DerRemover[arg_]:=
		If[MatchQ[arg,A_[B__][C_]/;(CovDQ@A||ParamD===A)],
			(*In this case thre is a derivative to strip off*)
			AppendTo[DerivList,Head@arg];
			First@arg,
			(*In this case there is no derivative so just return the same result*)
			arg
		];
	(*Apply DerRemover to Arg until no further change happens*)
	Arg=FixedPoint[DerRemover,Arg];
	(*Split ParamD derivatives into separate components*)
	DerivList=Flatten[DerivList//.ParamD[A_,B__]:>{ParamD[A],ParamD[B]}];
	(*output results*)
	{DerivList,Arg}
];



(* ::Section::Closed:: *)
(*Finish Up*)


(* ::Text:: *)
(*End the private context*)


End[]


(* ::Text:: *)
(*Protect symbols*)


Protect[$Splittings]
Protect[SplittingQ];
Protect[DecompositionQ];
Protect[DefSplitting];
Protect[UndefSplitting];
Protect[AllowDerivsOfUpMetric];
Protect[MinimizeMetricDummies];
Protect[ToDefaultNomenclature];
Protect[SplitExpression];
Protect[SplittingRulesListQ];
Protect[SplittingRules];
Protect[UseSplittingRules];
Protect[SplitComponents];
Protect[AddTensorDependencies];
Protect[InverseComposition];


(* ::Text:: *)
(*End package*)


EndPackage[]


(* ::Text:: *)
(**)
