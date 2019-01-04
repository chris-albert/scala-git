package io.lbert.git

import org.scalatest.{Matchers, WordSpec}

class GitSpec extends WordSpec with Matchers {

  "Object.determine" should {
    "get none if not enough bytes are passed" in {
      Git.Object.determine(Array()) shouldBe None
    }
    "get none if not valid object type" in {
      Git.Object.determine("hi".getBytes) shouldBe None
    }
    "get commit if type is commit" in {
      Git.Object.determine("commit 230".getBytes) shouldBe Some(Git.Object.Commit)
    }
    "get tree if type is tree" in {
      Git.Object.determine("tree 230".getBytes) shouldBe Some(Git.Object.Tree)
    }
    "get blob if type is blob" in {
      Git.Object.determine("blob 230".getBytes) shouldBe Some(Git.Object.Blob)
    }
    "get tag if type is tag" in {
      Git.Object.determine("tag 230".getBytes) shouldBe Some(Git.Object.Tag)
    }
  }
}
