from SCons.Script import AddOption, GetOption
import SCons.Script

from aptitude_configure_checks import RegisterCheck, ConfigureCheck
from aptitude_configure_utils import TryCompileCXX

@ConfigureCheck("Checking for ept::debtags::Tag")
def CheckForDebtagsTag(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  ept::debtags::Tag t;

  return 0;
}''')

@ConfigureCheck("Checking for ept::debtags::Facet")
def CheckForDebtagsFacet(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  ept::debtags::Facet f;

  return 0;
}''')

@ConfigureCheck("Checking whether ept::debtags::getTagsOfItem returns a set of strings.")
def CheckDebtagsGetTagsOfItemReturnsStrings(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  // This would always crash, but we only care if it type-checks.
  ept::debtags::Debtags DB;
  std::set<std::string> result = DB.getTagsOfItem("foo");

  return 0;
}''')

@ConfigureCheck("Checking whether ept::textsearch::TextSearch exists")
def CheckTextsearch(context):
    return TryCompileCXX(context, '''
#include <ept/textsearch/textsearch.h>

int main(int argc, char **argv)
{
  ept::textsearch::TextSearch db;

  db.docidByName("foo");

  return 0;
}''')

@ConfigureCheck("Checking whether ept/axi exists")
def CheckEptAxi(context):
    return TryCompileCXX(context, '''
#include <ept/axi/axi.h>

int main(int argc, char **argv)
{
  Xapian::Database db(ept::axi::path_db());

  return 0;
}''')

@ConfigureCheck("Checking whether ept::debtags::Vocabulary::tagData exists and returns ept::debtags::voc::TagData *")
def CheckEptDebtagsVocabularyTagData(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/vocabulary.h>

int main(int argc, char **argv)
{
  ept::debtags::Vocabulary vocabulary;
  const ept::debtags::voc::TagData *td = vocabulary.tagData("foo");

  // Use td so the compiler doesn\'t complain:
  return td == NULL ? 1 : 0;
}''')

@ConfigureCheck("Checking whether ept::debtags::Vocabulary::facetData exists and returns ept::debtags::voc::FacetData *")
def CheckEptDebtagsVocabularyFacetData(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/vocabulary.h>

int main(int argc, char **argv)
{
  ept::debtags::Vocabulary vocabulary;
  const ept::debtags::voc::FacetData *fd = vocabulary.facetData("foo");

  // Use fd so the compiler doesn\'t complain:
  return fd == NULL ? 1 : 0;
}''')

@ConfigureCheck("Checking whether ept::debtags::Tag::fullName() exists")
def CheckEptDebtagsTagFullName(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  ept::debtags::Tag t;

  std::string s = t.fullname();

  return 0;
}''')

@ConfigureCheck("Checking whether ept::debtags::Facet exists and supports description retrieval")
def CheckEptDebtagsFacetDescription(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  ept::debtags::Facet f;

  const std::string longDesc = f.longDescription();
  const std::string shortDesc = f.shortDescription();

  return 0;
}''')

@ConfigureCheck("Checking whether ept::debtags::Tag exists and supports description retrieval.")
def CheckEptDebtagsTagDescription(context):
    return TryCompileCXX(context, '''
#include <ept/debtags/debtags.h>

int main(int argc, char **argv)
{
  ept::debtags::Tag t;

  const std::string longDesc = t.longDescription();
  const std::string shortDesc = t.shortDescription();

  return 0;
}''')
