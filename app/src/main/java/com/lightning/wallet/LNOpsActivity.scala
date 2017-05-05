package com.lightning.wallet

import android.text.method.LinkMovementMethod
import android.widget.TextView
import android.os.Bundle
import android.view.View


class LNOpsActivity extends TimerActivity with ViewSwitch {
  lazy val lnOpsReadMore = findViewById(R.id.lnOpsReadMore).asInstanceOf[TextView]
  lazy val views = findViewById(R.id.lnOpsDescription) :: Nil

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_ln_ops)
    lnOpsReadMore setMovementMethod LinkMovementMethod.getInstance
  }

  def goChoosePeer(view: View) = ???
}
