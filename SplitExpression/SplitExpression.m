(* ::Package:: *)

(* ::Title:: *)
(*SplitExpression*)


(* ::Text:: *)
(*:Author:*)
(*Joseph Elliston*)


(* ::Text:: *)
(*:Summary:*)
(*The SplitExpression package extends the xTensor package to allow for simple decomposition of tensorial expressions by splitting tensorial indices into arbitrary parts. Functionality is provided to easily replace these tensors with user-defined rules. Single index components can be rewritten as parameters, avoiding unneccesary index proliferation. The motivation behing this package is to provide low-level functionality for operations such as the 3+1 split in General Relativity. *)


(* ::Text:: *)
(*:Context: *)
(*xAct`SplitExpression`*)


(* ::Text:: *)
(*:History:*)
(*Current version: v1.1.0 (2016.05.21)*)
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


xAct`SplitExpression`$Version={"1.1.0",{2016,5,21}};


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
Print["CopyRight (C) 2014-2016, Joseph Elliston, under the General Public License."];


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
        
       The tensorial indices that will be decomposed are assumed to live on a given tangent bundle. In xAct, these indices can be abstract (A) indices, basis (B) indices or individual component (C) indices. C-indices cannot be split since they are already written in terms of individual components. B-indices can arise whenever a basis has been defined, and A-indices are understood to be defined with respect to some fiducial basis. B- and C-indices are principally used in the xCoba package. In this SplitExpression package, we purposefully avoid using such basis or component indices and work only with A-indices. This choice is partially pragmatic, allowing us to use the greater functionality of xTensor in manipulating A-indices to yield simpler code. The only restriction imposed by choosing to only work with A-indices is the scenario with both A- and B-indices simultaneously present in the same VBundle, and this is a potentially confusing scenario in any case.
        
       The indices before and after the splitting must be defined first. Indices belong to a VBundle. If we also require a metric to raise and lower any of these indices, then xTensor demands that the VBundle is tangent to a manifold, and is known as a TangentVBundle. To avoid the complication of dealing with some VBundles being TangentVBundles and others not, we stipulate that all indices are defined on TangentVBundles. Since a TangentVBundle is created automatically when a manifold is defined, instead of working with VBundles we can work with manifolds; in SplitExpression one must define a manifold for the original space (along with its indices) and manifolds for all of the subspaces (along with their indices). Parameters, as discussed, will not have indices and so no manifolds need be defined for these. Returning to the '3+1' example, the discussion above means that at the start of the code one needs to define a spacetime manifold and a spacial manifold, with indices on each manifold, but no time manifold is required.
        
       For orientation putposes, let us now discuss the likely way in which the SplitExpression package will be used and summarise what the main functions do:
        
       1) One will begin by defining a manifold and some tensors to obtain the tensorial expression that one wants to split.
       2) Next one will need to define manifolds, indices and parameters that will appear in the expression after it is split.
       3) The splitting is defined using the function DefSplitting to define a splitting object. This function doesn't acutally do any splitting itself, but it stores the details of the splitting and associates these details to a name, e.g. 'MySplitting'. This allows the user to use as many different splittings as they like in the same code.
       4) The tensorial expression can then be split using the function 'SplitExpression'. If any metrics are defined then any associated covariant derivatives are turned into partial derivatives. Any dummy indices are then expanded out to be either parameters or indices of the defined sub-index ranges. If the input expression contains free indices then these are expanded into an array, with the dimension of the resulting array equal to the number of free indices. By means of illustration, consider a simple scalar example without free indices: We start with a four-dimensional spacetime manifold \[OpenCurlyQuote]M4\[CloseCurlyQuote] which we will split using the splitting \[OpenCurlyQuote]MySplitting\[CloseCurlyQuote] into a '3+1' 3-space + time form. Indices on M4 are '\[Mu],\[Nu],...', indices on the 3-dimensional spatial manifold M3 are 'i,j,...' and there is also a time parameter \[OpenCurlyQuote]t\[CloseCurlyQuote]. Using SplitExpression to perform MySplitting on a scalar quantity such as F[\[Mu],-\[Mu]] then yields F[t,-t]+F[i,-i]. This result contains invalid tensors because the parameter indices are inhomogeneous and not of standard form. This is resolved via an internal call to the function 'ToDefaultNomenclature' which we now discuss:
       5) 'ToDefaultNomenclature' acts on tensors with invalid parameter indices and, if not already defined, creates new split-tensors that are valid for the given indices. Following the previous example with a splitting called 'MySplitting', 'ToDefaultNomenclature' leads to the transformation F[t,-t]->F\[FormalCapitalS]MySplitting\[FormalCapitalU]t\[FormalCapitalD]t[] and F[i,-i]->F\[FormalCapitalS]MySplitting\[FormalCapitalM]M3\[FormalCapitalM]M3[i,-i], where the new tensors F\[FormalCapitalS]MySplitting\[FormalCapitalU]t\[FormalCapitalD]t and F\[FormalCapitalS]MySplitting\[FormalCapitalM]M3\[FormalCapitalM]M3 are in the 'default nomenclature' of the SplitExpression package. The default nomenclature is intended only as a placeholder; it may look ugly, but the tensor names clearly state how they are defined from their parent tensor. If we have defined a splitting called 'MySplitting' and a tensor called 'MyTensor' that has 'N' indices being split, then the default nomenclature for the split-tensors of MyTensor is a concetenation of names as <MyTensor>\[FormalCapitalS]<MySplitting><IndexForm1><IndexForm2>...<IndexFormN>, where <IndexForm> takes one of three forms:
        
       a) If the index is a contravariant (up) parameter called MyParameter then IndexForm = \[FormalCapitalU]MyParameter.
       b) If the index is a covariant (down) parameter called MyParameter then IndexForm = \[FormalCapitalD]MyParameter.
       c) If the index is an index (up or down) on some manifold MyManifold then IndexForm = \[FormalCapitalM]MyManifold.
        
       We incorporate the name of the splitting in the tensor name to ensure that all split-tensors are unique to each given splitting.
       6) One is able to stick with the default nomenclature. However, this may not be ideal for two reasons: Firstly, the resulting expressions look ugly and are longwinded. Secondly, it is likely that one would want to write out the split-tensors in terms of other tensors (such as in the '3+1' split in Relativity where the time-time component of the metric is rewritten as a function of the Lapse scalar and the Shift vector). To allow this to be done easily, the function SplittingRules may be used. This sets up a list of rules that are appended to the global variable $SplittingRules[MySplitting]. These rules can then be applied to an expression by using the function 'UseSplittingRules'. UseSplittingRules is automatically called by SplitExpression, so if one defines the splitting rules in advance of splitting the tensorial expression, then the default nomencature is never output.
        
       There is also a function ComputeCurvatureTensors to automatically compute components of curvature tensors as required.\[CloseCurlyDoubleQuote]


(* ::Subsection::Closed:: *)
(*Acknowledgements*)


(* ::Text:: *)
(*My thanks go to David Seery and the University of Sussex for allowing me to devote so much time into writing this package. *)
(*I am also indebted to Guido Pettinari for patiently sharing some of his vast knowledge of Mathematica.*)
(*--Joseph Elliston*)


(* ::Section::Closed:: *)
(*Public context*)


Unprotect[$InfoLevel];
$InfoLevel::usage = "Determines the amount of output generated by the functions in the SplitExpression package. 0 - all output minimised. 1 - text descriptions output detailing the current computation. 2 - intermediate outputs given. Be wary of using option 2 for complex calculations as too much output can crash \!\(\*
StyleBox[\"Mathematica\",\nFontSlant->\"Italic\"]\)!";

Unprotect[$Splittings];
$Splittings::usage = "Globally defined list of available splittings. If no splittings have been defined, $Splittings={}.";

Unprotect[SplittingQ];
SplittingQ::usage = "SplittingQ[symbol] gives True if symbol is registered as a splitting and False otherwise.";

Unprotect[DecompositionQ];
DecompositionQ::usage = "DecompositionQ[OldManifold,Decomposition] gives True if Decomposition is of the correct form to be used as a decomposition argument to DefSplitting. DecompositionQ checks the form of Decomposition and checks that it has a total dimensionality equal to that of OldManifold. Decomposition must be a list of Manifolds or Parameters all of which must have been predefined.";

Unprotect[DefSplitting];
DefSplitting::usage = "DefSplitting[MySplitting,OldManifold->Decomposition] defines a prescription for splitting indices over smaller summations or individual parameters. Decomposition must be a list of Manifolds or Parameters all of which must have been predefined. We use parameters in this way to avoid the unwanted clutter indices that would otherwise live on single-dimensional manifolds, though single-dimension manifolds can be used if wanted. 'MySplitting' becomes a new splitting object and should not be previously defined.";
DefSplitting::exists = "The symbol `1` is already defined."

Unprotect[UndefSplitting];
UndefSplitting::usage = "UndefSplitting[MySplitting] undefines the Splitting called MySplitting.";

Unprotect[ToDefaultNomenclature];
ToDefaultNomenclature::usage = "ToDefaultNomenclature[MySplitting][MyExpression] is a key function in the SplitExpression package. If MyExpression is naively split into block form and assigned new indices related to the sub-manifolds of the splitting, then this expression is not usually valid because the indices do not match the indice configuration of the original tensor. ToDefaultNomenclature overcomes this issue by defining necessary new split-tensors with the appropriate index configurations. These new split tensors are defined with the appropriate rank, by removing parameter indices at the point of definition, but other indices remain. The name of the resulting split-tensors encodes the original index configuration in a verbose but reliable way. If we have defined a splitting called MySplitting and a tensor called MyTensor that has N indices being split by MySplitting, then the default nomenclature for a given split-tensor of MyTensor is a concetenation of names associated to its indices as <MyTensor>\[FormalCapitalS]<MySplitting><IndexForm1><IndexForm2>...<IndexFormN>, where <IndexForm> takes one of three forms:
                              If the index is a contravariant (up) parameter called MyParameter then IndexForm = \[FormalCapitalU]MyParameter.
                              If the index is a covariant (down) parameter called MyParameter then IndexForm = \[FormalCapitalD]MyParameter.
                              If the index is an index (up or down) on some manifold MyManifold then IndexForm = \[FormalCapitalM]MyManifold.";
ToDefaultNomenclature::nsplitting = "`1` is not a valid splitting object defined with DefSplitting.";
ToDefaultNomenclature::multiple = "There should only be one argument in each of the two brackets of ToDefaultNomenclature[Splitting][Expression].";
ToDefaultNomenclature::null = "One or both of the arguments to ToDefaultNomenclature are empty. The correct method has two non-empty arguments as ToDefaultNomenclature[Splitting][Expression].";

Unprotect[CanonicalOutputQ];
CanonicalOutputQ::usage = "Boolean option for the functions SplitExpression, SplitComponents, UseSplittingRules and ComputeCurvatureTensors that determines whether or not the result should be put into canonical form. Default value is True. This can have a significant impact on computation time.";

Unprotect[Fun];
Fun::usage = "Optional function argument in SplitExpression that is operated on the expression being split before the splitting happens.";

Unprotect[ComponentsListQ];
ComponentsListQ::usage = "Boolean option for SplitExpression, determining whether the output is in an array of a comonent list format. Default is False.";

Unprotect[SplitExpression];
SplitExpression::usage = "SplitExpression[MySplitting,CanonicalOutputQ->True,Fun->Identity,ComponentsListQ->False][MyExpression] splits indices in MyExpression according to the splitting MySplitting. If MyExpression contains N-free indices then the output is an N-dimensional array. The expansion will only work if the indices in MyExpression that are in MySplitting's VBundle are abstract (A) indices as used by default in xTensor. SplitExpression makes an internal call to ToDefaultNomenclature in order to produce valid split-tensors. The usage messages for the optional arguments cover their purposes.";
SplitExpression::multiple = "There should only be one argument in the second bracket of SplitExpression[MySplitting,<optional arguments>][MyExpression].";
SplitExpression::null = "One or both of the arguments to SplitExpression are empty. The correct method has two non-empty arguments as SplitExpression[MySplitting,<optional arguments>][MyExpression].";
SplitExpression::invinds = "The input expression contains indices that are not supported as input arguments to the function SplitExpression. The expansion will only work for abstract (A) indices in the TangentVBundle being expanded, though other indices may exist in other VBundles or as labels. This error could also indicate that the first bracket in SplitExpression[MySplitting,<optional arguments>][MyExpression] has been incorrectly populated.";

Unprotect[SplittingRulesListQ];
SplittingRulesListQ::usage = "Returns True if its argument is of the correct form for being a SplittingRulesList, otherwise resturns False. SplittingRulesListQ checks the form of the expression and its index syntax. All Tensors contained within SplittingRulesList are presumed to be already defined. SplittingRulesList should be a list of single depth and each element should be a rule of the form TensorName[Indices]->Result. 'Indices' are the indices of the tensor rule being defined, and these can be contravariant or covariant and can also be parameters. 'Result' should be tensorially correct (without any parameter indices) and must contain the same free (non-parameter) indices as 'Indices' in the same up/down configuration.";

Unprotect[SplittingRules];
SplittingRules::usage = "SplittingRules[MySplitting,MySplittingRulesList] allows the user to define rules for how the split-tensors will be rewritten after application of the SplitExpression function. This is useful because the default nomenclature for split-tensors is often longwinded and also one may want to write out the split-tensors in terms of other tensors. This substitution is automatic if SplittingRules is called before SplitExpression. SplittingRules appends rules to the global variable $SplittingRules[MySplitting]. These rules are then applied to an expression by using the function 'UseSplittingRules'. See SplittingRulesListQ for the syntax needed for SplittingRulesList.";
SplittingRules::multiple = "Internal error: Multiple instances of the same transformation rule found: `1`.";

Unprotect[ComputeCurvatureTensors];
ComputeCurvatureTensors::usage = "ComputeCurvatureTensors[MySplitting,CanonicalOutputQ->True, Target -> All, AllComponentsQ -> False] evaluates the Christoffel terms, Riemann tensor, Ricci tensor, Ricci scalar and Einstein tensors associated with the metric being used for the index splitting. The Target optional argument may be used to reduce the amount of computation to only compute those tensors required to calculate a target tensor. Target can table values of All, Christoffel, Riemann, Ricci, RicciScalar and Einstein. By default, only the components are found for indices in their default configurations, but all index configurations can be computed by setting AllComponentsQ -> True.";
ComputeCurvatureTensors::null = "The argument to ComputeCurvatureTensors cannot be empty.";
ComputeCurvatureTensors::multiple = "ComputeCurvatureTensors[MySplitting,<optional arguments>] has one or more arguments appearing in a single argument bracket only.";

Unprotect[Target];
Target::usage = "Optional argument for the ComputeCurvatureTensors function to determine which tensors to compute."

Unprotect[AllComponentsQ];
AllComponentsQ::usage = "Optional argument for the ComputeCurvatureTensors function to determine whether or not to compute all tensor components or only those in default index positions."

Unprotect[UseSplittingRules];
UseSplittingRules::usage = "UseSplittingRules[MySplitting,CanonicalOutputQ->True][MyExpression] applies the rules $SplittingRules[MySplitting] on MyExpression. These rules are defined using the SplittingRules function and are used to rewrite the split-tensors in a form that is not the default nomenclature.";
UseSplittingRules::argx = "Aside from using the CanonicalOutputQ optional argument, there should only be one argument in each of the two brackets of UseSplittingRules[MySplitting][MyExpression].";

Unprotect[AddTensorDependencies];
AddTensorDependencies::usage = "AddTensorDependencies[{SomeTensors},{SomeParameters}] adds SomeParameters as dependencies of SomeTensors.";
AddTensorDependencies::invarg = "The input arguments are invalid. The first argument should be a list of tensors and the second argument a list of parameters, all of which have already been defined.";

Unprotect[InverseComposition];
InverseComposition::usage = "InverseComposition[expr] will output a list of the form {{Deriv1,Deriv2,Deriv3,...},Arg} where expr=Deriv1@Deriv2@Deriv3@...@Arg, where Deriv can be any covariant dderivative for which CovD returns True, or a parameter derivative ParamD. InverseComposition therefore strips derivatives off of the argument Arg and returns the result in a decomposed form. This process is essentailly the inverse of Mathematica's 'Composition' function, hence the name. For any argument that does not fit the description of being at least one derivative acting on an argument, the output will be {{},expr}.";


(* ::Text:: *)
(*The default value for $Splittings is an empty list.*)


$Splittings={};


(* ::Text:: *)
(*Define the variable InfoLevel to control the output of useful debugging messages using the private InfoPrint function. *)


(* ::Item:: *)
(*InfoLevel=0 --- All output messages are minimised.*)


(* ::Item:: *)
(*InfoLevel=1 --- Text descriptions of the code about to be run are produced.*)


(* ::Item:: *)
(*InfoLevel=2 --- Intermediate outputs are also produced.*)


$InfoLevel = 0;


(* ::Text:: *)
(*Edit the definition of ParamD so that it always preceeds PD. This is necessary for SeparateMetric to work properly.*)


ParamD/:PD[x_]@ParamD[y_]@z_:=ParamD[y]@PD[x]@z;


(* ::Section::Closed:: *)
(*Private Context*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Debugging*)


(* ::Text:: *)
(*Define the function InfoPrint[stuff,level] that prints out 'stuff' only if InfoLevel is sufficient.*)


SetNumberOfArguments[InfoPrint,2];
InfoPrint[expr_,level_]:=If[level<=$InfoLevel,Print[expr];];


(* ::Subsection::Closed:: *)
(*SplittingQ*)


(* ::Input:: *)
(*?SplittingQ*)


(* ::Text:: *)
(*Default result is False.*)


SetNumberOfArguments[SplittingQ,1];
SplittingQ[_]:=False;


(* ::Subsection::Closed:: *)
(*DecompositionQ*)


(* ::Input:: *)
(*?DecompositionQ*)


SetNumberOfArguments[DecompositionQ, 2];


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


(* ::Text:: *)
(*In other cases the default value is False.*)


DecompositionQ[_, _] := False;


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
		If[ValueQ@Global`$SplittingRules[splitting],Global`$SplittingRules[splitting]=.];

		(*Remove splitting*)
		ClearAll[splitting];
		InfoPrint["Finally $Splittings = ",2];
		InfoPrint[$Splittings,2];		
	];


(* ::Text:: *)
(*Then the case there the argument is incorrect:*)


UndefSplitting[NotSplitting_] := Message[UndefSplitting::invalid, NotSplitting, "splitting"];


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
   	(*Almost the entire content of this function is the renaming of tensors to default nomenclature using one complicated rule.
   	Note that this applies so long as the tensor "AA" includes one or more indices that were affected by "Splitting".
		This dependency is determined using the HostsOf function which relates "AA" to its dependent VBundles*)
   	newexpr = newexpr /. AA_[BB__] /; (xTensorQ@AA && MemberQ[HostsOf@AA, VBundleOf[splitting]]) :>
      		Module[{OldVBPositions, ChangedVBPositions, NewName, NewDependencies, NewTensorInds, NonDummyInds},
       			
       			(*OldVBPositions gives the locations of the indices that were originally in VBundleOf[splitting]*)
       			OldVBPositions = Position[SlotsOfTensor[AA] /. -CC_ :> CC, VBundleOf[splitting]];
       
       			(*NewName gives the name of the tensor that AA[BB] will turn it into in default nomenclature*)
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
   	newexpr = newexpr //. PD[-BB_?ParameterQ][CC__] :> ParamD[BB][CC]
   ];


(* ::Text:: *)
(*Case where first argument is not a valid splitting*)


(* ::Subsection::Closed:: *)
(*SplitExpression*)


(* ::Input:: *)
(*?SplitExpression*)


(* ::Text:: *)
(*Case when all arguments are correct:*)


Options[SplitExpression] = {CanonicalOutputQ -> True, Fun->Identity, ComponentsListQ->False};

SplitExpression[splitting_?SplittingQ,OptionsPattern[]][expr_] :=
  Module[{newexpr, ii, dummies, NewInds, ComponentRules, ExpandFirstDummy, free, xx, IndArray, tmpdelta},

		(*FIND FREE INDICES*)
    	newexpr = expr;    
		free = List @@ IndicesOf[Free, VBundleOf[splitting]][newexpr];
		InfoPrint["SplitExpression::Free indices are:",2];
		InfoPrint[free,2];

    	(*MANIPULATE INPUT IF THERE IS A METRIC ASSOCIATED TO THE TANGENT VBUNDLE BEING SPLIT*) 
    	If[UnsameQ[MetricOf[splitting], {}],
     		(*Lower any contravariant derivatives*)
			InfoPrint["SplitExpression::Lowering all contravariant derivatives...", 1];
     		newexpr = newexpr //. (CovDOf[splitting])[a_][A__] /; (UpIndexQ@a && AIndexQ@a) :> Module[{tmp},
            			(MetricOf[splitting][a, tmp] (CovDOf[splitting])[-tmp]@A /. tmp -> NewIndexIn[VBundleOf[splitting]])
            			] // ReplaceDummies // SameDummies // ScreenDollarIndices;
     		InfoPrint[newexpr, 2];
     
     		(*Expand out any covariant derivatives associated with the given tangentVBundle as partial derivatives PD*)
     		InfoPrint["SplitExpression::Expanding covariant derivatives into partial derivatives...", 1];
     		newexpr = ToCanonical[ChangeCovD[newexpr, CovDOf[splitting], PD], UseMetricOnVBundle -> None] // Simplify;
     		InfoPrint[newexpr, 2];
		];	
    	
		(*APPLY THE FUNCTION "Fun" TO THE EXPRESSION to allow for custom simplification*)
		InfoPrint["SplitExpression::Applying the function 'Fun'...", 1];
		newexpr = NoScalar@MapAll[OptionValue[Fun],newexpr];
		InfoPrint["SplitExpression::Result after applying 'Fun' is:", 2];
		InfoPrint[newexpr, 2];

    	(*DECOMPOSITION OF DUMMY INDICES*)
    	(*dummies is the list of remaining contravariant dummies in newexpr*)
		InfoPrint["SplitExpression::Expanding out the dummy indices...", 1];
    	dummies = List @@ IndicesOf[VBundleOf[splitting], Dummy, Up][newexpr];
		InfoPrint["SplitExpression::Dummy indices are:", 2];
		InfoPrint[dummies, 2];
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
     		InfoPrint["SplitExpression::Expanding dummy index " <> ToString@ii <> " of " <> ToString@Length@dummies <> "...", 1];
     		(*Now apply ExpandFirstDummy to each term of newexpr recursively until it has finished*)
     		newexpr = If[Head@newexpr === Plus,
       			ExpandFirstDummy[#, dummies[[ii]]] & /@ Expand[newexpr],
       			ExpandFirstDummy[newexpr, dummies[[ii]]]
       		];
     		InfoPrint[newexpr, 2];
     	];
    
    	(*DECOMPOSITION OF FREE INDICES VIA A MULTI-DIMENSIONAL ARRAY*)
    	(*Only compute this section if there are some free indices*)
    	If[Length@free =!= 0,
			InfoPrint["SplitExpression::Creating the array of indices to replace the free indices...", 1];
     		(*We replace each free indice with all possible indices of the SubVBundles of the splitting.
			We mimmic the way the calculation would be done by hand, where each free indice is decomposed into a unique set of indices associated to 
			the various SubVBundles of the splitting. To make the multi-dimensional array of new indices, we will use the 'Outer' function
			but we must first create separate lists of replacement indices for each free index, with no re-use of indices between terms.*)
     		NewInds = Table[
				(*'xx' is a list of free indices for applying to each term. Each new free index is assigned the xx'th defined index in each SubVBundle, 
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
     		(*Turn these indices into indice replacement rules*)
     		IndArray = Map[Thread[free -> #] &, IndArray, {Length@free}];
     		InfoPrint["New index array to replace the free indices is: " <> ToString@IndArray, 2];

			(*When the ComponentsListQ===True, extract the index replacement rules here*)
     		If[OptionValue[ComponentsListQ], ComponentRules = Nest[Flatten[#, 1] &, IndArray, Length@free - 1];];
     		InfoPrint["Index replacement rules are: " <> ToString@IndArray, 2];
     		
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
     		InfoPrint["SplitExpression::Substituting delta tensor with tmpdelta tensor...", 1];
     		newexpr = newexpr /. delta[a_, b_] :> tmpdelta[a, b];
     		InfoPrint[newexpr, 2];
     		(*Now we can finally replace the free indices*)
     		InfoPrint["SplitExpression::Substituting for free indices...", 1];
     		newexpr = Map[ReplaceIndex[Evaluate[newexpr], #] &, IndArray, {Length@free}];
     		InfoPrint[newexpr, 2];
     		(*Returning to the original delta tensor as the indices have valid forms, and undefine the delta tensor*)
     		InfoPrint["SplitExpression::Restoring tmpdelta to delta...", 1];
     		newexpr = newexpr /. tmpdelta -> delta;
     		InfoPrint[newexpr, 2];
     		UndefTensor[tmpdelta];
     		InfoPrint["SplitExpression::Free indices are now split.", 1];
     	];
    	
    	(*SWITCH TO AUTOMATED DECOMPOSED TENSOR NOMENCLATURE*)
    	InfoPrint["SplitExpression::Switching to default nomenclature...", 1];
    	newexpr = Map[ToDefaultNomenclature[splitting][#] &, newexpr, {Length@free}];
    	InfoPrint[newexpr, 2];
    	InfoPrint["SplitExpression::Applying splitting rules...", 1];	
    	newexpr = UseSplittingRules[splitting,CanonicalOutputQ->OptionValue[CanonicalOutputQ]][newexpr];
    	InfoPrint[newexpr, 2];
		If[OptionValue[CanonicalOutputQ],
	    	InfoPrint["SplitExpression::Canonicalising...", 1];
			newexpr = Map[ToCanonical[#,Simplify@UseMetricOnVBundle->None]&, newexpr, {Length@free}];
		];
    	InfoPrint["SplitExpression::Simplifying and outputting the result...", 1];
    	newexpr = Map[ScreenDollarIndices@ReplaceDummies@ContractMetric@NoScalar@#&, newexpr, {Length@free}];

		(*DEFINE THE OUTPUT FORMAT*)
		(*If the output format is a component list, we ensure that the output is of the same form for any rank of expr*)
		If[OptionValue[ComponentsListQ],
			If[free=!={},
				ColumnForm@Thread[Rule[HoldForm@expr /. # & /@ ComponentRules, Flatten@newexpr]],
				ColumnForm@List@Rule[expr,newexpr]
			],
			newexpr
		]    
    ] /; (IndicesOf[VBundleOf[splitting]][expr] === IndicesOf[VBundleOf[splitting], AIndex][expr]);


(* ::Text:: *)
(*Set the number of arguments:*)


(*Cases where one or both arguments are null*)
SplitExpression[][] := Message[SplitExpression::null];
SplitExpression[__][] := Message[SplitExpression::null];
SplitExpression[][__] := Message[SplitExpression::null];
(*Multiple arguments*)
SplitExpression[__][_, A__] := Message[SplitExpression::multiple];


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
			CleanedRules = SplittingRulesList//. a_?xTensorQ[b___,-_?ParameterQ,c___]:>a[b,c];
			CleanedRules = CleanedRules//. a_?xTensorQ[b___,_?ParameterQ,c___]:>a[b,c];
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
      	(*If the splitting rules list is not defined, make it an empty list*)
      	If[Unevaluated@Global`$SplittingRules[splitting]===Global`$SplittingRules[splitting],Global`$SplittingRules[splitting] = {}];
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
        		If you can do better, please email the author...*)

        		(*pos is the position of the terms of name that clash with LHSpattern*)
        		pos = Flatten@Position[MatchQ[ToString@FullForm@First@First@#, ToString@FullForm@Evaluate@LHSpattern] & /@ Global`$SplittingRules[splitting], True];
        		(*pos should have zero or 1 entry. Anything else signifies an internal error.*)
        		If[Length@pos > 1, Message[SplittingRules::multiple, Global`$SplittingRules[splitting][[pos]]]];
        		(*If there is a single term that clashes then remove this*)
        		Global`$SplittingRules[splitting] = Delete[Global`$SplittingRules[splitting], pos];
        		
        		(*add the new term, of the form HoldPattern[LHS]=Module[{dummies},RHS], achieved through the IndexRuleDelayed function
				note that the scalar head is added if the result is a scalar, to ensure that powers of scalars can be substituted for*)
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
(*ComputeCurvatureTensors*)


(* ::Input:: *)
(*?ComputeCurvatureTensors*)


(* ::Text:: *)
(*Define the case where all works properly. *)


Options[ComputeCurvatureTensors] = {CanonicalOutputQ -> True, Target -> All, AllComponentsQ -> False};

ComputeCurvatureTensors[splitting_?SplittingQ,OptionsPattern[]] :=
        Module[{FindSplitComponents,OriginalUpValues},
     		(*Only do anything is there is a metric associated to the splitting*)      	
     		If[UnsameQ[MetricOf[splitting], {}],
       			(*define the function FindSplitComponents that finds the various component splitting rules 
					for a given tensor. This function takes a second argument, which is the function that needs
					to be applied to the given tensor to find it in terms of lower level curvature quantities.
					For example, we find the Ricci Tensor from the Riemann Tensor.*)
					FindSplitComponents[nameString_,RHSfun_,AllComponentsQinternal_] :=
						Module[{name,inds,ten,result,splittingRulesLHS,splittingRulesRHS,tenList,LHSlist,RHSlist,ii,ladder,LHSnew,RHSnew},
							(*First find the name and indices of what we are finding components for, and call the expression 'ten'*)
							name = Symbol[nameString<>ToString@CovDOf@splitting];
							inds = SlotsOfTensor@name/.a_?VBundleQ:>NewIndexIn@a;
							ten = name@@(inds);
							(*Use SplitExpression to find the splitting, and use the component output form so that we retain the input format for the LHS*)
							result = ReleaseHold@SplitExpression[splitting,CanonicalOutputQ->OptionValue[CanonicalOutputQ],ComponentsListQ->True,Fun->Composition[SeparateMetric[],RHSfun]][ten];
							splittingRulesLHS=#[[1]]&/@List@@@First@result;
							InfoPrint["ComputeCurvatureTensors::Calculating "<>nameString<>" tensor components", 0];
							InfoPrint[splittingRulesLHS, 1];
							splittingRulesRHS=Simplify@Flatten[#[[2]]&/@List@@@First@result];
							(*Combine LHS and RHS into rules for the SplittingRules function*)
							InfoPrint[ColumnForm[Rule@@@Partition[#,2]&@Riffle[splittingRulesLHS,splittingRulesRHS]],2];
							SplittingRules[splitting,Rule@@@Partition[#,2]&@Riffle[splittingRulesLHS,splittingRulesRHS]];
							
							(*FIND OTHER COMPONENTS IF WANTED*)
							If[AllComponentsQinternal,
								(*Keep a list of the tensor components found so far*)
								tenList = {ten};
								(*Cover all possibilities my moving each index up/down in turn and applying to all previous results*)
								For[ii=1,ii<=Length@inds,ii++,
									(*define the metric that acts as a ladder operator to raise/lower the ii'th index*)
									ladder = MetricOf[splitting]@@{-inds[[ii]],If[Head@inds[[ii]]===Times,1 #,-1 #]&@NewIndexIn@VBundleOfIndex@inds[[ii]]};
									(*find the LHS and RHS for this new set of index configurations*)
									LHSnew = #[[1]]&/@List@@@First@ReleaseHold@SplitExpression[splitting,CanonicalOutputQ->OptionValue[CanonicalOutputQ],ComponentsListQ->True][#]&/@(ContractMetric[ladder #,MetricOf[splitting]]&/@tenList);
									InfoPrint["ComputeCurvatureTensors::Calculating:", 1];
									InfoPrint[LHSnew, 1];
									RHSnew = (ScreenDollarIndices@ToCanonical[Flatten@SplitExpression[splitting,CanonicalOutputQ->OptionValue[CanonicalOutputQ]][ladder #],UseMetricOnVBundle->None])&/@tenList;
									InfoPrint[ColumnForm[Rule@@@Partition[#,2]&@Riffle[Flatten@LHSnew,Flatten@RHSnew]], 2];
									(*Update tenlist so that it is ready for the next loop iteration*)
									tenList = Flatten@{tenList, ContractMetric[ladder #,MetricOf[splitting]]&/@tenList};
									(*Find the splitting rules*)
									SplittingRules[splitting,Rule@@@Partition[#,2]&@Riffle[Flatten@LHSnew,Flatten@RHSnew]];
								];
							];
						];
					
					(*NOW SIMPLY APPLY THIS FUNCTION FOR EACH TENSOR WANTED*)
					If[AnyTrue[{All,Einstein,RicciScalar,Ricci,Riemann,Christoffel},OptionValue[Target]===#&],FindSplitComponents["Christoffel",ChristoffelToMetric,OptionValue[AllComponentsQ]]];
					If[AnyTrue[{All,Einstein,RicciScalar,Ricci,Riemann},OptionValue[Target]===#&],FindSplitComponents["Riemann",RiemannToChristoffel,OptionValue[AllComponentsQ]]];
					If[AnyTrue[{All,Einstein,RicciScalar,Ricci},OptionValue[Target]===#&],FindSplitComponents["Ricci",RiemannToChristoffel,OptionValue[AllComponentsQ]]];
					If[AnyTrue[{All,RicciScalar},OptionValue[Target]===#&],FindSplitComponents["RicciScalar",RiemannToChristoffel,OptionValue[AllComponentsQ]]];
					If[AnyTrue[{All,Einstein},OptionValue[Target]===#&],FindSplitComponents["Einstein",EinsteinToRicci,OptionValue[AllComponentsQ]]];					
       		];
     	];



(* ::Text:: *)
(*Set the number of arguments:*)


(*Cases where an argument is null*)
ComputeCurvatureTensors[] := Message[ComputeCurvatureTensors::null];
(*Multiple arguments*)
ComputeCurvatureTensors[___][___] := Message[ComputeCurvatureTensors::multiple];


(* ::Text:: *)
(*Case where first argument is not a valid splitting*)


ComputeCurvatureTensors[nsplitting_,___] := Message[ComputeCurvatureTensors::unknown, "splitting", nsplitting];


(* ::Subsection::Closed:: *)
(*UseSplittingRules*)


(* ::Input:: *)
(*?UseSplittingRules*)


(* ::Text:: *)
(*Actual function, and all that this does is to apply splitting rules if they are defined.*)


Options[UseSplittingRules] = {CanonicalOutputQ -> True};

UseSplittingRules[splitting_?SplittingQ, OptionsPattern[]][expr_]:=
Module[{fun,newexpr},
	(*Check that there are rules to use*)
	If[!ValueQ@Global`$SplittingRules[splitting],
		expr,
		newexpr = expr;
		fun[arg_]:=ToCanonical[arg//.Global`$SplittingRules[splitting],UseMetricOnVBundle->None]//ScreenDollarIndices;
		(*first act fun on tensors withing power heads, then on everything else*)
		InfoPrint["UseSplittingRules::Applying splittingrules to terms involving powers of tensors...",1];
		newexpr = newexpr//.Power[A_?xTensorQ[],B_]:>NoScalar@Power[Scalar@fun@A[],B];

		InfoPrint["UseSplittingRules::Applying splittingrules to the other terms...",1];
		newexpr = newexpr//.Global`$SplittingRules[splitting];
		If[OptionValue[CanonicalOutputQ],
			InfoPrint["UseSplittingRules::Canonicalising...",1];
			newexpr = ToCanonical[newexpr,UseMetricOnVBundle->None];
		];
		InfoPrint["UseSplittingRules::Screening dollar indices to complete the UseSplittingRules function...",1];
		ScreenDollarIndices[newexpr]
	]
];


(* ::Text:: *)
(*Set the number of arguments:*)


UseSplittingRules[][___] := Message[UseSplittingRules::argx];
UseSplittingRules[___][] := Message[UseSplittingRules::argx];
UseSplittingRules[_, A__][_] := Message[UseSplittingRules::argx];
UseSplittingRules[_][_, A__] := Message[UseSplittingRules::argx];


(* ::Text:: *)
(*The case where splitting is not an existing Splitting*)


UseSplittingRules[nsplitting_][_] := Message[UseSplittingRules::unknown, "splitting", nsplitting];


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


Protect[$Splittings];
Protect[SplittingQ];
Protect[DecompositionQ];
Protect[DefSplitting];
Protect[UndefSplitting];
Protect[ToDefaultNomenclature];
Protect[SplitExpression];
Protect[SplittingRulesListQ];
Protect[SplittingRules];
Protect[ComputeCurvatureTensors];
Protect[Target];
Protect[AllComponentsQ];
Protect[UseSplittingRules];
Protect[CanonicalOutputQ];
Protect[Fun];
Protect[ComponentsListQ];
Protect[AddTensorDependencies];
Protect[InverseComposition];


(* ::Text:: *)
(*End package*)


EndPackage[]


(* ::Text:: *)
(**)
