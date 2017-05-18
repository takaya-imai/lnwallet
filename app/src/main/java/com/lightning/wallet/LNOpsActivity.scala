package com.lightning.wallet

import com.lightning.wallet.R.string._
import android.widget.{Button, TextView}
import android.text.method.LinkMovementMethod
import android.os.Bundle
import android.view.View


class LNOpsActivity extends TimerActivity { me =>
  lazy val lnOpsDescription = findViewById(R.id.lnOpsDescription).asInstanceOf[TextView]
  lazy val lnOpsAction = findViewById(R.id.lnOpsAction).asInstanceOf[Button]
  def goBitcoin(view: View) = me exitTo classOf[BtcActivity]

  // Initialize this activity, method is run once
  override def onCreate(savedInstanceState: Bundle) =
  {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.activity_ln_ops)
    lnOpsDescription setMovementMethod LinkMovementMethod.getInstance
    showInfo
  }

  def showInfo = {
    def goCreateChannel = me exitTo classOf[LNStartActivity]
    lnOpsAction setOnClickListener onButtonTap(goCreateChannel)
    lnOpsDescription setText ln_ops_description
    lnOpsAction setText ln_ops_start
  }
}
